{-# LANGUAGE ConstraintKinds, KindSignatures, DataKinds, ScopedTypeVariables, DeriveFunctor #-}

module BlobStore where

import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.Builder as Builder
import qualified Data.Attoparsec as A
import Crypto.Hash
import qualified Data.HashMap.Strict as HM
import Data.IORef
import Data.Hashable
import Data.Byteable
import qualified Data.Cache.LRU as LRU
import Control.Exception
import Control.DeepSeq (force)
import Control.Applicative ((<$>), (<$))
import Data.Monoid

data Address = SHA512Key B.ByteString
    deriving (Eq, Ord, Show)

addressBuilder :: Address -> Builder.Builder
addressBuilder (SHA512Key key) = Builder.word8 1 <> Builder.byteString (toBytes key)

addressParse :: A.Parser Address
addressParse = SHA512Key <$> (A.word8 1 >> A.take 64)

instance Hashable Address where
    hashWithSalt salt (SHA512Key k) = hashWithSalt salt (toBytes k)

data Decorated = Decorated Address L.ByteString
    deriving (Show)

address :: Decorated -> Address
address (Decorated a _) = a

decorate :: L.ByteString -> Decorated
decorate blob = Decorated (SHA512Key key) blob
    where key = toBytes (hashlazy blob :: Digest SHA512)

class Object a where
    serialize :: a -> Decorated
    deserialize :: Decorated -> Either String a

instance Object L.ByteString where
    serialize = decorate
    deserialize (Decorated _ x) = Right x

instance Object B.ByteString where
    serialize = serialize . L.fromStrict
    deserialize = fmap L.toStrict . deserialize

data StorageLevel a = Unknown | Cached { storedObject :: a } | Stored { storedObject :: a }
    deriving (Functor, Show)

data RawStore f = RawStore
    { rawStore :: Decorated -> f (StorageLevel ())
    , rawLoad :: Address -> f (StorageLevel Decorated)
    }

newtype Store f a = Store (RawStore f)

store :: Object a => Store f a -> a -> f (StorageLevel ())
store (Store st) x = rawStore st (serialize x)

load :: (Functor f, Object a) => Store f a -> Address -> f (StorageLevel (Either String a))
load (Store st) a = fmap deserialize <$> rawLoad st a

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
duplicated (<&>) (<|>) a b = RawStore { rawStore = \o -> rawStore a o <&> rawStore b o
                                   , rawLoad = \i -> rawLoad a i <|> rawLoad b i }

duplicatedSerial :: Monad f => RawStore p1 f -> RawStore p2 f -> RawStore p2 f
duplicatedSerial = duplicated (>>) orM

cache :: Monad f => RawStore Cached f -> RawStore p f -> RawStore p f
cache = duplicatedSerial

multi :: Monad f => (Address -> f (RawStore p f)) -> RawStore p f
multi locate = RawStore { rawStore = doStore, rawLoad = doLoad }
    where doStore o@(Decorated a _) = locate a >>= (`rawStore` o)
          doLoad a = locate a >>= (`rawLoad` a)
-}

memoryStore :: IORef (HM.HashMap Address L.ByteString) -> RawStore IO
memoryStore mapRef = RawStore { rawStore = doStore, rawLoad = doLoad }
    where doStore (Decorated a o) = atomicModifyIORef' mapRef (\m -> (HM.insert a o m, Stored ()))
          doLoad a = maybe Unknown (Stored . Decorated a) . HM.lookup a <$> readIORef mapRef

newMemoryStore :: IO (RawStore IO)
newMemoryStore = memoryStore <$> newIORef HM.empty

lruCache :: IORef (LRU.LRU Address L.ByteString) -> RawStore IO
lruCache cacheRef = RawStore { rawStore = doStore, rawLoad = doLoad }
    where doStore (Decorated a o) = atomicModifyIORef' cacheRef (\m -> (LRU.insert a o m, Cached ()))
          doLoad a = maybe Unknown (Cached . Decorated a) <$> atomicModifyIORef' cacheRef (LRU.lookup a)

newLRUCache :: Maybe Integer -> IO (RawStore IO)
newLRUCache len = lruCache <$> newIORef (LRU.newLRU len)

fsStore :: FilePath -> RawStore IO
fsStore dir = RawStore { rawStore = doStore, rawLoad = doLoad }
    where
        addrPath (SHA512Key k) = dir ++ "/O_" ++ show k
        doStore (Decorated a o) = Stored () <$ L.writeFile (addrPath a) o
        doLoad a = do m <- try $ force <$> L.readFile (addrPath a)
                      return $ case m of
                        Left (_ :: IOException) -> Unknown
                        Right x -> Stored (Decorated a x)
