{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Rolling where

import Data.Word
import Data.Bits
import Data.List
import Test.QuickCheck hiding ((.&.))

prime :: Word64
prime = 16411

newtype ModPrime = P Word64
  deriving (Show, Eq, Ord, Enum, Real, Integral)

mk :: Word64 -> ModPrime
mk x = P $ x `rem` prime

instance Num ModPrime where
  fromInteger x | x >= 0 = mk $ fromInteger x
                | otherwise = negate . mk . fromInteger $ -x
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

mask :: Word64
mask = 1 `shiftL` 13 - 1

droppedMultiplier :: ModPrime
droppedMultiplier = size ^ (window - 1)

roll1 :: ModPrime -> (ModPrime, ModPrime) -> ModPrime
roll1 h (n,o) = size * (h - droppedMultiplier * o) + n

roll :: [ModPrime] -> [ModPrime]
roll xs = tail $ scanl roll1 0 withOld
    where withOld = zip xs (replicate window 0 ++ xs)

windowOrBigger :: Gen [ModPrime]
windowOrBigger = sized $ \n -> do k <- choose (window, window + n)
                                  vector k

prop_roll = forAll windowOrBigger $ \x -> last (roll x) == last (roll (drop (length x - window) x))

rollsplit :: [ModPrime] -> [Int]
rollsplit xs = diff $ 0 : indices ++ [length xs]
    where indices = findIndices (\(P x) -> x .&. mask == mask) $ roll xs
          diff = zipWith (-) =<< tail -- from the dungeons
