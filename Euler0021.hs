module Euler0021 where

import Common.Factorize
import Common.Math
import Common.List

d :: Integral a => a -> a
d = sum . pdivisors

amicable :: Integral a => a -> Bool
amicable n = n /= d n && d (d n) == n

solution :: IO Integer
solution = return . sum . filter amicable $ natPlus `below` 10000
