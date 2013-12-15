{-# LANGUAGE ConstraintKinds, KindSignatures, DataKinds, ScopedTypeVariables, DeriveFunctor, RankNTypes #-}

module BlobStore where

import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.Builder as Builder
import qualified Data.Attoparsec as A
import qualified Data.ByteString.Base64.URL as Base64U
import qualified Crypto.Hash.SHA512 as SHA512
import qualified Data.HashMap.Strict as HM
import Data.IORef
import Data.Hashable
import Data.Byteable
import qualified Data.Cache.LRU as LRU
import Control.Exception
import Control.DeepSeq (force)
import Control.Applicative ((<$>), (<$))
import Data.Monoid
import Control.Lens

data Address = SHA512Key B.ByteString
    deriving (Eq, Ord, Show)

instance Byteable Address where
    toBytes (SHA512Key x) = x

addressBuilder :: Address -> Builder.Builder
addressBuilder (SHA512Key key) = Builder.word8 1 <> Builder.byteString (toBytes key)

addressParse :: A.Parser Address
addressParse = SHA512Key <$> (A.word8 1 >> A.take 64)

instance Hashable Address where
    hashWithSalt salt (SHA512Key k) = hashWithSalt salt (toBytes k)

data Decorated = Decorated Address L.ByteString
    deriving (Eq, Show)

address :: Decorated -> Address
address (Decorated a _) = a

decorate :: L.ByteString -> Decorated
decorate blob = Decorated (SHA512Key key) blob
    where key = SHA512.hashlazy blob

class Object a where
    serialize :: a -> Decorated
    deserialize :: Decorated -> Either String a

instance Object L.ByteString where
    serialize = decorate
    deserialize (Decorated _ x) = Right x

instance Object B.ByteString where
    serialize = serialize . L.fromStrict
    deserialize = fmap L.toStrict . deserialize

data StorageQuality = Cached | Permanent
    deriving (Show)

data StorageLevel e a = NoValidObject e | Stored StorageQuality a
    deriving (Functor, Show)

joinStorageLevel :: StorageLevel e (Either e a) -> StorageLevel e a
joinStorageLevel (NoValidObject e) = NoValidObject e
joinStorageLevel (Stored _ (Left e)) = NoValidObject e
joinStorageLevel (Stored q (Right x)) = Stored q x

data Store f a = Store
    { store :: a -> f (StorageLevel String Address)
    , load :: Address -> f (StorageLevel String a)
    }

type RawStore f = Store f Decorated

objectStore :: (Functor f, Monad f, Object a) => RawStore f -> Store f a
objectStore raw = Store { store = doStore, load = doLoad }
    where doStore x = store raw (serialize x)
          doLoad a = joinStorageLevel . fmap deserialize <$> load raw a

prismStore :: (Functor f, Monad f) => Prism' s a -> Store f s -> Store f a
prismStore p st = Store { store = doStore, load = doLoad }
    where doStore o = store st (review p o)
          doLoad a = joinStorageLevel . fmap (maybe (Left "Excluded type") Right . preview p) <$> load st a

{-
-- possible implementation of <|>
orM :: Monad f => f (Maybe a) -> f (Maybe a) -> f (Maybe a)
orM m n = do x <- m
             case x of
               Just _ -> return x
               Nothing -> n

type Rel a = a -> a -> a

duplicated :: Monad f => Rel (f ()) -> Rel (f (Maybe Decorated)) ->
                         RawStore p1 f -> RawStore p2 f -> RawStore p3 f
duplicated (<&>) (<|>) a b = RawStore { store = \o -> store a o <&> store b o
                                   , load = \i -> load a i <|> load b i }

duplicatedSerial :: Monad f => RawStore p1 f -> RawStore p2 f -> RawStore p2 f
duplicatedSerial = duplicated (>>) orM

cache :: Monad f => RawStore Cached f -> RawStore p f -> RawStore p f
cache = duplicatedSerial

multi :: Monad f => (Address -> f (RawStore p f)) -> RawStore p f
multi locate = RawStore { store = doStore, load = doLoad }
    where doStore o@(Decorated a _) = locate a >>= (`store` o)
          doLoad a = locate a >>= (`load` a)
-}

memoryStore :: IORef (HM.HashMap Address L.ByteString) -> RawStore IO
memoryStore mapRef = Store { store = doStore, load = doLoad }
    where doStore (Decorated a o) = atomicModifyIORef' mapRef (\m -> (HM.insert a o m, Stored Permanent a))
          doLoad a = maybe (NoValidObject "NoValidObject address") (Stored Permanent . Decorated a) . HM.lookup a <$> readIORef mapRef

newMemoryStore :: IO (RawStore IO)
newMemoryStore = memoryStore <$> newIORef HM.empty

lruCache :: IORef (LRU.LRU Address L.ByteString) -> RawStore IO
lruCache cacheRef = Store { store = doStore, load = doLoad }
    where doStore (Decorated a o) = atomicModifyIORef' cacheRef (\m -> (LRU.insert a o m, Stored Cached a))
          doLoad a = maybe (NoValidObject "Not in cache") (Stored Cached . Decorated a) <$> atomicModifyIORef' cacheRef (LRU.lookup a)

newLRUCache :: Maybe Integer -> IO (RawStore IO)
newLRUCache len = lruCache <$> newIORef (LRU.newLRU len)

fsStore :: FilePath -> RawStore IO
fsStore dir = Store { store = doStore, load = doLoad }
    where
        addrPath (SHA512Key k) = dir ++ "/O_" ++ B8.unpack (Base64U.encode k)
        doStore (Decorated a o) = Stored Permanent a <$ L.writeFile (addrPath a) o
        doLoad a = do m <- try $ force <$> L.readFile (addrPath a)
                      return $ case m of
                        Left (e :: IOException) -> (NoValidObject $ show e)
                        Right x -> Stored Permanent (Decorated a x)

verify :: Monad f => RawStore f -> RawStore f
verify st = Store { store = doStore, load = doLoad }
    where doStore x@(Decorated a o) | checkAddress a o = store st x
                                    | otherwise        = return $ NoValidObject "Non-matching SHA-512"
          doLoad a = do s <- load st a
                        return $ case s of
                          Stored _ (Decorated a' o) | a == a' && checkAddress a o -> s
                                                    | otherwise -> NoValidObject "Non-matching SHA-512"
                          NoValidObject _ -> s
          checkAddress (SHA512Key k) o = k == SHA512.hashlazy o
