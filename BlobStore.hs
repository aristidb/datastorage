{-# LANGUAGE ConstraintKinds, KindSignatures, DataKinds, DeriveFunctor #-}

import Crypto.Hash
import qualified Data.HashMap.Strict as HM
import Data.IORef
import Data.Hashable
import Data.Byteable
import Data.Binary

data Address = SHA512Key (Digest SHA512)
    deriving (Eq, Show)

instance Hashable Address where
    hashWithSalt salt (SHA512Key k) = hashWithSalt salt (toBytes k)

data Object a = Object Address a
    deriving (Functor, Show)

address :: Object a -> Address
address (Object a _) = a

makeObject :: Binary a => a -> Object a
makeObject obj = Object (SHA512Key key) obj
    where key = hashlazy (encode obj)

data Permanence = Cached | Stored

data Store (p :: Permanence) f a = Store
    { store :: Object a -> f ()
    , load :: Address -> f (Maybe (Object a))
    }

-- possible implementation of <|>
orM :: Monad f => f (Maybe a) -> f (Maybe a) -> f (Maybe a)
orM m n = do x <- m
             case x of
               Just _ -> return x
               Nothing -> n

type Rel a = a -> a -> a

duplicated :: Monad f => Rel (f ()) -> Rel (f (Maybe (Object a))) ->
                         Store p1 f a -> Store p2 f a -> Store p3 f a
duplicated (<&>) (<|>) a b = Store { store = \o -> store a o <&> store b o
                                   , load = \i -> load a i <|> load b i }

duplicatedSerial :: Monad f => Store p1 f a -> Store p2 f a -> Store p2 f a
duplicatedSerial = duplicated (>>) orM

cache :: Monad f => Store Cached f a -> Store p f a -> Store p f a
cache = duplicatedSerial

multi :: Monad f => (Address -> f (Store p f a)) -> Store p f a
multi locate = Store { store = xstore, load = xload }
    where xstore o@(Object a _) = locate a >>= (`store` o)
          xload a = locate a >>= (`load` a)

memoryStore :: IORef (HM.HashMap Address a) -> Store Stored IO a
memoryStore mapRef = Store { store = xstore, load = xload }
    where xstore (Object a o) = atomicModifyIORef' mapRef (\m -> (HM.insert a o m, ()))
          xload a = (fmap (Object a) . HM.lookup a) `fmap` readIORef mapRef

newMemoryStore :: IO (Store Stored IO a)
newMemoryStore = memoryStore `fmap` newIORef HM.empty
