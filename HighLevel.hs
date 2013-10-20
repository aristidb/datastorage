{-# LANGUAGE DataKinds, KindSignatures, GADTs, RankNTypes #-}
module HighLevel where

import Pipes
import Prelude hiding (read, splitAt)

data Object (m :: * -> *) a = Object -- opaque for now

read :: Object m a -> Producer a m ()
read = undefined

write :: Producer a m () -> Object m a
write = undefined

slice :: Int -> Int -> Object m a
slice = undefined

splitAt :: Int -> Object m a -> (Object m a, Object m a)
splitAt = undefined

append :: Object m a -> Object m a -> Object m a
append = undefined
