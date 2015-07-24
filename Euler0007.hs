module Euler0007 where

import Common.Primes

nthPrime n = primes !! (n-1)

solution :: IO Integer
solution = return $ nthPrime 10001
