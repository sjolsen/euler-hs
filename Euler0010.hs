module Euler0010 where

import Common.Primes
import Common.List

solution = sum $ primes `below` 2000000
