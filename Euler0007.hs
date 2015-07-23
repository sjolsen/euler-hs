module Euler0007 where

import Common.Primes

nthPrime n = primes !! (n-1)

solution = nthPrime 10001
