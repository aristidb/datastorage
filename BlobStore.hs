{-# LANGUAGE ConstraintKinds #-}

import qualified Data.ByteString as B

data Address = SHA512Key B.ByteString

data Object = Object Address B.ByteString

makeObject :: B.ByteString -> Object
makeObject = undefined

data Store f = Store
    { cache :: Object -> f ()
    , store :: Object -> f ()
    , load :: Address -> f Object
    }

class Conj f where
    conj :: f a -> f b -> f ()

class Disj f where
    disj :: f a -> f a -> f a

type Both f = (Conj f, Disj f)

duplicated :: Both f => Store f -> Store f -> Store f
duplicated a b = Store { cache = \o -> cache a o `conj` cache b o
                       , store = \o -> store a o `conj` store b o
                       , load = \i -> load a i `disj` load b i }

multi :: Monad f => (Address -> f (Store f)) -> Store f
multi locate = Store { cache = xcache, store = xstore, load = xload }
    where xcache o@(Object a _) = locate a >>= (`cache` o)
          xstore o@(Object a _) = locate a >>= (`store` o)
          xload a = locate a >>= (`load` a)
