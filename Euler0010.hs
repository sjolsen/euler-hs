module Euler0010 where

import Common.Primes
import Common.List

solution :: IO Integer
solution = return . sum $ primes `below` 2000000
