{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Rolling where

import Data.Word
import Data.Bits
import Test.QuickCheck

prime :: Word64
prime = 16411

newtype ModPrime = P Word64
  deriving (Show, Eq, Ord, Enum, Real, Integral)

mk :: Word64 -> ModPrime
mk x = P $ x `rem` prime

instance Num ModPrime where
  fromInteger x = mk $ fromInteger x
  P x + P y = mk $ x + y
  P x * P y = mk $ x * y
  negate (P x) = mk $ prime - x
  abs = id
  signum (P x) = P $ signum x

instance Bounded ModPrime where
  minBound = 0
  maxBound = -1

instance Arbitrary ModPrime where
  arbitrary = arbitraryBoundedEnum

window :: Int
window = 128

size :: ModPrime
size = 256

droppedMultiplier :: ModPrime
droppedMultiplier = size ^ (window - 1)

roll1 :: ModPrime -> (ModPrime, ModPrime) -> ModPrime
roll1 h (n,o) = size * (h - droppedMultiplier * o) + n

roll :: [ModPrime] -> [ModPrime]
roll xs = scanl roll1 0 (zip xs (replicate window 0 ++ xs))

windowOrBigger :: Gen [ModPrime]
windowOrBigger = sized $ \n -> do k <- choose (window, window + n)
                                  vector k

prop_roll = forAll windowOrBigger $ \x -> last (roll x) == last (roll (drop (length x - window) x))
