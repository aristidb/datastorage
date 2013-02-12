module Test where

import Rolling
import Criterion.Main

myf1 :: Pure
myf1 = whnf (rollSum 407 65) 32
