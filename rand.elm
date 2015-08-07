module Rand where

import Bitwise

multiplier = 1103515245   -- 1 | 3 | 5 | 7 | 9 | 15 | 
increment  = 12345        -- 3 5 823
modulus    = 4294967296
mask       = 2147418112

next : Int -> Int
next x = ((multiplier * x) + increment) % modulus

maskOff n = Bitwise.shiftRightLogical (Bitwise.and mask n) 16

