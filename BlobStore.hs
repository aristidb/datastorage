{-# LANGUAGE ConstraintKinds, KindSignatures, DataKinds, ScopedTypeVariables #-}

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

data Object = Object Address L.ByteString
    deriving (Show)

address :: Object -> Address
address (Object a _) = a

makeObject :: L.ByteString -> Object
makeObject blob = Object (SHA512Key key) blob
    where key = hashlazy blob

data StorageLevel a = Unknown | Cached { storedObject :: a } | Stored { storedObject :: a }
    deriving (Show)

data Store f = Store
    { store :: Object -> f (StorageLevel ())
    , load :: Address -> f (StorageLevel Object)
    }

{-
-- possible implementation of <|>
orM :: Monad f => f (Maybe a) -> f (Maybe a) -> f (Maybe a)
orM m n = do x <- m
             case x of
               Just _ -> return x
               Nothing -> n

type Rel a = a -> a -> a

duplicated :: Monad f => Rel (f ()) -> Rel (f (Maybe Object)) ->
                         Store p1 f -> Store p2 f -> Store p3 f
duplicated (<&>) (<|>) a b = Store { store = \o -> store a o <&> store b o
                                   , load = \i -> load a i <|> load b i }

duplicatedSerial :: Monad f => Store p1 f -> Store p2 f -> Store p2 f
duplicatedSerial = duplicated (>>) orM

cache :: Monad f => Store Cached f -> Store p f -> Store p f
cache = duplicatedSerial

multi :: Monad f => (Address -> f (Store p f)) -> Store p f
multi locate = Store { store = xstore, load = xload }
    where xstore o@(Object a _) = locate a >>= (`store` o)
          xload a = locate a >>= (`load` a)
-}

memoryStore :: IORef (HM.HashMap Address L.ByteString) -> Store IO
memoryStore mapRef = Store { store = xstore, load = xload }
    where xstore (Object a o) = atomicModifyIORef' mapRef (\m -> (HM.insert a o m, Stored ()))
          xload a = maybe Unknown (Stored . Object a) . HM.lookup a <$> readIORef mapRef

newMemoryStore :: IO (Store IO)
newMemoryStore = memoryStore <$> newIORef HM.empty

lruCache :: IORef (LRU.LRU Address L.ByteString) -> Store IO
lruCache cacheRef = Store { store = xstore, load = xload }
    where xstore (Object a o) = atomicModifyIORef' cacheRef (\m -> (LRU.insert a o m, Cached ()))
          xload a = maybe Unknown (Cached . Object a) <$> atomicModifyIORef' cacheRef (LRU.lookup a)

newLRUCache :: Maybe Integer -> IO (Store IO)
newLRUCache len = lruCache <$> newIORef (LRU.newLRU len)

fsStore :: FilePath -> Store IO
fsStore dir = Store { store = xstore, load = xload }
    where
        addrPath (SHA512Key k) = dir ++ "/O_" ++ show k
        xstore (Object a o) = Stored () <$ L.writeFile (addrPath a) o
        xload a = do m <- try $ force <$> L.readFile (addrPath a)
                     return $ case m of
                       Left (_ :: IOException) -> Unknown
                       Right x -> Stored (Object a x)
