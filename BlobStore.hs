{-# LANGUAGE ConstraintKinds #-}

import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L
import qualified Crypto.Hash.SHA512 as SHA512

data Address = SHA512Key B.ByteString
    deriving (Show)

data Object = Object Address L.ByteString
    deriving (Show)

makeObject :: L.ByteString -> Object
makeObject blob = Object (SHA512Key key) blob
    where key = SHA512.hashlazy blob

data Store f = Store
    { cache :: Object -> f ()
    , store :: Object -> f ()
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
                         Store f -> Store f -> Store f
duplicated (<&>) (<|>) a b = Store { cache = \o -> cache a o <&> cache b o
                            , store = \o -> store a o <&> store b o
                            , load = \i -> load a i <|> load b i }

duplicatedSerial :: Monad f => Store f -> Store f -> Store f
duplicatedSerial = duplicated (>>) orM

multi :: Monad f => (Address -> f (Store f)) -> Store f
multi locate = Store { cache = xcache, store = xstore, load = xload }
    where xcache o@(Object a _) = locate a >>= (`cache` o)
          xstore o@(Object a _) = locate a >>= (`store` o)
          xload a = locate a >>= (`load` a)
