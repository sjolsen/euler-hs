module Euler0003 where

import Common.Factorize
import Common.Primes

solution = maximum . fmap fst . factorizeWith primes $ 600851475143
