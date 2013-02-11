module Rolling where

import Data.Word
import Data.Bits
import Test.QuickCheck

prime :: Word64
prime = 16411

window :: Int
window = 128

powModPrime_naive :: Word64 -> Word64 -> Word64
powModPrime_naive m n = fromInteger $ (toInteger m ^ toInteger n) `rem` toInteger prime

powModPrime :: Word64 -> Word64 -> Word64
powModPrime m 0 = 1
powModPrime m 1 = m `rem` prime
powModPrime m n = let (q,p) = n `quotRem` 2
                      k = powModPrime m q
                      f = if p == 0 then 1 else m
                  in (k * k * f) `rem` prime

prop_powModPrime_correct = forAll (choose (0, prime - 1)) $ \a ->
                           forAll (choose (0, 50000)) $ \b ->
                           powModPrime a b == powModPrime_naive a b

roll1 :: Word64 -> (Word64, Word64) -> Word64
roll1 h (n,o) = undefined
