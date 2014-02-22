{-# LANGUAGE CPP #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

module IndexTree (Index(..), IndexTree(..), SizeList(..), makeIndex) where

import Succinct.Dictionary
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Internal.Check as Ck
import Data.Monoid

#define BOUNDS_CHECK(f) Ck.f __FILE__ __LINE__ Ck.Bounds

data Index = Fixed {-# UNPACK #-} !Int {-# UNPACK #-} !Int
           | Mapped Rank9
    deriving (Show)

instance Access Bool Index where
    size (Fixed n _) = n
    size (Mapped m) = size m

    Fixed n m ! i = BOUNDS_CHECK(checkIndex) "Index.!" i (n*m)
                  $ i `mod` m == 0
    Mapped m ! i = m ! i

instance Select0 Index
instance Select1 Index

instance Dictionary Bool Index where
    rank a (Mapped m) i = rank a m i
    rank True (Fixed _ m) i = (i - 1) `div` m + 1
    rank False t i = i - rank True t i

    select a (Mapped m) i = select a m i
    select True (Fixed _ m) i = i * m
    select False (Fixed _ m) i = let (d, r) = (i - 1) `divMod` (m - 1)
                                 in d * m + r + (m-1)

data IndexTree l = Absent
                 | Leaf Int
                 | Node [l] Index (V.Vector (IndexTree l))
    deriving (Show)

instance Access Int (IndexTree l) where
    size Absent = 0
    size (Leaf n) = n
    size (Node _ ix _) = size ix

    -- depth
    Absent ! _i = error "Absent"
    Leaf _n ! _i = 0
    Node _ _ix _us ! _ = undefined

newtype SizeList = SizeList [Int]

instance Bitwise SizeList where
    -- TODO: more efficient implementation?
    bitwise (SizeList []) = U.empty
    bitwise (SizeList (n:xs)) = U.generate n (Bit . (==0)) <> bitwise (SizeList xs)

makeIndex :: [Int] -> Index
makeIndex = Mapped . rank9 . SizeList
