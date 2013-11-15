{-# LANGUAGE ConstraintKinds #-}

import qualified Data.ByteString as B

data Address = SHA512Key B.ByteString

data Object = Object Address B.ByteString

makeObject :: B.ByteString -> Object
makeObject = undefined

data Store f = Store
    { cache :: Object -> f ()
    , store :: Object -> f ()
    , load :: Address -> f (Maybe Object)
    }

orM :: Monad f => f (Maybe a) -> f (Maybe a) -> f (Maybe a)
orM m n = do x <- m
             case x of
               Just _ -> return x
               Nothing -> n

duplicated :: Monad f => (f () -> f () -> f ()) -> Store f -> Store f -> Store f
duplicated (<>) a b = Store { cache = \o -> cache a o <> cache b o
                            , store = \o -> store a o <> store b o
                            , load = \i -> load a i `orM` load b i }

multi :: Monad f => (Address -> f (Store f)) -> Store f
multi locate = Store { cache = xcache, store = xstore, load = xload }
    where xcache o@(Object a _) = locate a >>= (`cache` o)
          xstore o@(Object a _) = locate a >>= (`store` o)
          xload a = locate a >>= (`load` a)
