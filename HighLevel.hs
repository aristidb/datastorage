{-# LANGUAGE DataKinds, KindSignatures, GADTs, RankNTypes #-}
module HighLevel where

import Data.ByteString as B
import Prelude hiding (read)

data ObjectKind = V | S

data Object :: ObjectKind -> * where
  Stored :: ByteString -> Object a
  Take :: Int -> Object a -> Object V

view :: Object S -> Object a
view (Stored bs) = Stored bs

read :: Object a -> ByteString
read (Stored bs) = bs
read (Take n x) = B.take n $ read x

{-
store :: Object a -> IO (Object S)
store (Stored bs) = return (Stored bs)
store (Take _n _x) = undefined
-}
