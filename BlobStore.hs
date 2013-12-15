{-# LANGUAGE ConstraintKinds, KindSignatures, DataKinds, ScopedTypeVariables, DeriveFunctor, RankNTypes, ViewPatterns #-}

module BlobStore where

import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as B8
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

class Addressable o where
    address :: o -> Address

instance Addressable B.ByteString where
    address o = SHA512Key $ SHA512.hash o

data Decorated = Decorated Address B.ByteString
    deriving (Eq, Show)

instance Addressable Decorated where
    address (Decorated a _) = a

instance Byteable Decorated where
    toBytes (Decorated _ o) = o

type Put a = (Addressable a, Byteable a)

decorate :: (Addressable o, Byteable o) => o -> Decorated
decorate o = Decorated (address o) (toBytes o)

class Get a where
    undecorate :: Decorated -> Either String a
    undecorate (Decorated a x) = unroll a x

    unroll :: Address -> B.ByteString -> Either String a
    unroll a x = undecorate (Decorated a x)

instance Get Decorated where
    undecorate = Right

instance Get B.ByteString where
    unroll _ x = Right x

data StorageQuality = Cached | Permanent
    deriving (Show)

data StorageLevel e a = NoValidObject e | Stored StorageQuality a
    deriving (Functor, Show)

joinStorageLevel :: StorageLevel e (Either e a) -> StorageLevel e a
joinStorageLevel (NoValidObject e) = NoValidObject e
joinStorageLevel (Stored _ (Left e)) = NoValidObject e
joinStorageLevel (Stored q (Right x)) = Stored q x

checkStorageLevel :: (a -> Maybe e) -> StorageLevel e a -> StorageLevel e a
checkStorageLevel _ s@(NoValidObject _) = s
checkStorageLevel f s@(Stored _ x) = maybe s NoValidObject (f x)

stored :: Get a => StorageQuality -> Address -> B.ByteString -> StorageLevel String a
stored q a = joinStorageLevel . Stored q . unroll a

undecorateStored :: Get a => StorageLevel String Decorated -> StorageLevel String a
undecorateStored = joinStorageLevel . fmap undecorate

data Store f i o = Store
    { store :: i -> f (StorageLevel String Address)
    , load :: Address -> f (StorageLevel String o)
    }

objectStore :: (Functor f, Monad f, Put i, Get o) => Store f Decorated Decorated -> Store f i o
objectStore raw = Store { store = doStore, load = doLoad }
    where doStore x = store raw (decorate x)
          doLoad a = joinStorageLevel . fmap undecorate <$> load raw a

-- TODO: use non-simple Prism?
prismStore :: (Functor f, Monad f) => Prism' s a -> Store f s s -> Store f a a
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

memoryStore :: (Put i, Get o) => IORef (HM.HashMap Address B.ByteString) -> Store IO i o
memoryStore mapRef = Store { store = doStore, load = doLoad }
    where doStore (decorate -> Decorated a o) = atomicModifyIORef' mapRef (\m -> (HM.insert a o m, Stored Permanent a))
          doLoad a = maybe (NoValidObject "Unknown address") (stored Permanent a) . HM.lookup a <$> readIORef mapRef

newMemoryStore :: (Put i, Get o) => IO (Store IO i o)
newMemoryStore = memoryStore <$> newIORef HM.empty

lruCache :: (Put i, Get o) => IORef (LRU.LRU Address B.ByteString) -> Store IO i o
lruCache cacheRef = Store { store = doStore, load = doLoad }
    where doStore (decorate -> Decorated a o) = atomicModifyIORef' cacheRef (\m -> (LRU.insert a o m, Stored Cached a))
          doLoad a = maybe (NoValidObject "Not in cache") (stored Cached a) <$> atomicModifyIORef' cacheRef (LRU.lookup a)

newLRUCache :: (Put i, Get o) => Maybe Integer -> IO (Store IO i o)
newLRUCache len = lruCache <$> newIORef (LRU.newLRU len)

fsStore :: (Put i, Get o) => FilePath -> Store IO i o
fsStore dir = Store { store = doStore, load = doLoad }
    where
        addrPath (SHA512Key k) = dir ++ "/O_" ++ B8.unpack (Base64U.encode k)
        doStore (decorate -> Decorated a o) = Stored Permanent a <$ B.writeFile (addrPath a) o
        doLoad a = do m <- try $ B.readFile (addrPath a)
                      return $ case m of
                        Left (e :: IOException) -> (NoValidObject $ show e)
                        Right x -> stored Permanent a x

verify :: (Functor f, Monad f, Put i, Get o) => Store f Decorated Decorated -> Store f i o
verify st = Store { store = doStore, load = doLoad }
    where doStore (decorate -> x@(Decorated a _)) = check (== a) "Non-matching return address" <$> store st x
          doLoad a = undecorateStored . check (\(Decorated a' o) -> a == a' && checkAddress a o) "Non-matching SHA-512" <$> load st a
          check f e = checkStorageLevel (\x -> if f x then Nothing else Just e)
          checkAddress (SHA512Key k) o = k == SHA512.hash o
