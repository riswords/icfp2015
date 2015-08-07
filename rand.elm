module Rand where

import Bitwise

multiplier = 1103515245   -- 1 | 3 | 5 | 7 | 9 | 15 | 
increment  = 12345        -- 3 5 823
modulus    = 4294967296
mask       = 2147418112

-- Seed -> (Next, Seed)
next : Int -> (Int, Int)
next x = let nextSeed = ((multiplier * x) + increment) % modulus
         in  (maskOff nextSeed, nextSeed)

maskOff n = Bitwise.shiftRightLogical (Bitwise.and mask n) 16

