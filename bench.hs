import Rolling

import Criterion.Main

testhash :: (Num a, Enum a) => (a -> a -> a -> a) -> a -> a
testhash f i = last $ map (f 407 65) [0 .. i]
{-# INLINE testhash #-}

main :: IO ()
main = defaultMain [
         bgroup "rolling hash" [
           bench "roll1" $ whnf (testhash roll1) 10000,
           bench "rollSum" $ whnf (testhash rollSum) 10000,
           bench "rollEdward" $ whnf (testhash rollEdward) 10000
         ]
       ]
