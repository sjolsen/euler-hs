module Euler0012 where

import Data.List
import Control.Arrow
import Common.Math
import Common.Factorize
import Common.Combinatorics
import Common.Primes

triangles :: Integral a => [a]
triangles = fmap fst $ iterate (uncurry (+) &&& (+1) . snd) (0, 1)

nfactors :: Integral a => a -> Integer
nfactors = mcombinations . factorizeWith primes

solve :: Integer -> Maybe Integer
solve n = find ((> n) . nfactors) triangles

solution :: IO Integer
solution = return . maybe undefined id $ solve 500
