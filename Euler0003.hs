module Euler0003 where

import Common.Factorize
import Common.Primes

solution :: IO Integer
solution = return . maximum . fmap fst . factorize $ 600851475143
