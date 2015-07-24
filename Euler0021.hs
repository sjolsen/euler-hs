module Euler0021 where

import Common.Factorize
import Common.Math
import Common.List

d :: Integral a => a -> a
d 0 = undefined
d 1 = 1
d n = sum (divisors n) - n

amicable :: Integral a => a -> Bool
amicable n = n /= d n && d (d n) == n

solution = sum . filter amicable $ natPlus `below` 10000
