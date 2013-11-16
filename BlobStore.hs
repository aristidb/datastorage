{-# LANGUAGE ConstraintKinds, KindSignatures, DataKinds #-}

import qualified Data.ByteString.Lazy as L
import Crypto.Hash
import qualified Data.HashMap.Strict as HM
import Data.IORef
import Data.Hashable
import Data.Byteable

data Address = SHA512Key (Digest SHA512)
    deriving (Eq, Show)

instance Hashable Address where
    hashWithSalt salt (SHA512Key k) = hashWithSalt salt (toBytes k)

data Object = Object Address L.ByteString
    deriving (Show)

address :: Object -> Address
address (Object a _) = a

makeObject :: L.ByteString -> Object
makeObject blob = Object (SHA512Key key) blob
    where key = hashlazy blob

data Permanence = Cached | Stored

data Store (p :: Permanence) f = Store
    { store :: Object -> f ()
    , load :: Address -> f (Maybe Object)
    }

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

memoryStore :: IORef (HM.HashMap Address L.ByteString) -> Store Stored IO
memoryStore mapRef = Store { store = xstore, load = xload }
    where xstore (Object a o) = atomicModifyIORef' mapRef (\m -> (HM.insert a o m, ()))
          xload a = (fmap (Object a) . HM.lookup a) `fmap` readIORef mapRef

newMemoryStore :: IO (Store Stored IO)
newMemoryStore = memoryStore `fmap` newIORef HM.empty
