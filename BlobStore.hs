{-# LANGUAGE ConstraintKinds, KindSignatures, DataKinds, ScopedTypeVariables #-}

module BlobStore where

import qualified Data.ByteString.Lazy as L
import Crypto.Hash
import qualified Data.HashMap.Strict as HM
import Data.IORef
import Data.Hashable
import Data.Byteable
import qualified Data.Cache.LRU as LRU
import Control.Exception
import Control.DeepSeq (force)
import Control.Applicative ((<$>), (<$))

data Address = SHA512Key (Digest SHA512)
    deriving (Eq, Ord, Show)

instance Hashable Address where
    hashWithSalt salt (SHA512Key k) = hashWithSalt salt (toBytes k)

data Decorated = Decorated Address L.ByteString
    deriving (Show)

address :: Decorated -> Address
address (Decorated a _) = a

decorate :: L.ByteString -> Decorated
decorate blob = Decorated (SHA512Key key) blob
    where key = hashlazy blob

data StorageLevel a = Unknown | Cached { storedObject :: a } | Stored { storedObject :: a }
    deriving (Show)

data Store f = Store
    { store :: Decorated -> f (StorageLevel ())
    , load :: Address -> f (StorageLevel Decorated)
    }

{-
-- possible implementation of <|>
orM :: Monad f => f (Maybe a) -> f (Maybe a) -> f (Maybe a)
orM m n = do x <- m
             case x of
               Just _ -> return x
               Nothing -> n

type Rel a = a -> a -> a

duplicated :: Monad f => Rel (f ()) -> Rel (f (Maybe Decorated)) ->
                         Store p1 f -> Store p2 f -> Store p3 f
duplicated (<&>) (<|>) a b = Store { store = \o -> store a o <&> store b o
                                   , load = \i -> load a i <|> load b i }

duplicatedSerial :: Monad f => Store p1 f -> Store p2 f -> Store p2 f
duplicatedSerial = duplicated (>>) orM

cache :: Monad f => Store Cached f -> Store p f -> Store p f
cache = duplicatedSerial

multi :: Monad f => (Address -> f (Store p f)) -> Store p f
multi locate = Store { store = xstore, load = xload }
    where xstore o@(Decorated a _) = locate a >>= (`store` o)
          xload a = locate a >>= (`load` a)
-}

memoryStore :: IORef (HM.HashMap Address L.ByteString) -> Store IO
memoryStore mapRef = Store { store = xstore, load = xload }
    where xstore (Decorated a o) = atomicModifyIORef' mapRef (\m -> (HM.insert a o m, Stored ()))
          xload a = maybe Unknown (Stored . Decorated a) . HM.lookup a <$> readIORef mapRef

newMemoryStore :: IO (Store IO)
newMemoryStore = memoryStore <$> newIORef HM.empty

lruCache :: IORef (LRU.LRU Address L.ByteString) -> Store IO
lruCache cacheRef = Store { store = xstore, load = xload }
    where xstore (Decorated a o) = atomicModifyIORef' cacheRef (\m -> (LRU.insert a o m, Cached ()))
          xload a = maybe Unknown (Cached . Decorated a) <$> atomicModifyIORef' cacheRef (LRU.lookup a)

newLRUCache :: Maybe Integer -> IO (Store IO)
newLRUCache len = lruCache <$> newIORef (LRU.newLRU len)

fsStore :: FilePath -> Store IO
fsStore dir = Store { store = xstore, load = xload }
    where
        addrPath (SHA512Key k) = dir ++ "/O_" ++ show k
        xstore (Decorated a o) = Stored () <$ L.writeFile (addrPath a) o
        xload a = do m <- try $ force <$> L.readFile (addrPath a)
                     return $ case m of
                       Left (_ :: IOException) -> Unknown
                       Right x -> Stored (Decorated a x)
