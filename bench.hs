import Rolling

import Criterion.Main

main :: IO ()
main = defaultMain [
         bgroup "rolling hash" [
           bench "roll1" $ whnf (roll1 407) (65, 32)
         ]
       ]
