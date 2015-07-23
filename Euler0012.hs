module Euler0012 where

import Data.List
import Data.Bifunctor
import Control.Arrow
import Common.Primes
import Common.Math
import Common.List

-- Combinatorics

type Multiset a = [(a, Integer)]

mcombinations :: Multiset a -> Integer
mcombinations = product . fmap ((+1) . snd)

-- Primes and factorization

factorize :: Integral a => a -> Multiset a
factorize n = factorizeWith (primes `upTo` isqrt n) n
  where
    factorizeWith _      0 = [(0, 1)]
    factorizeWith _      1 = []
    factorizeWith []     n = [(n, 1)]
    factorizeWith (p:ps) n = case n `withoutFactor` p of
      (r, 0) -> factorizeWith ps r
      (r, m) -> (p, m) : factorizeWith ps r

    withoutFactor n p = case n `divMod` p of
      (q, 0) -> bimap id (+1) $ q `withoutFactor` p
      _      -> (n, 0)

unfactorize :: Integral a => Multiset a -> a
unfactorize = product . fmap (uncurry (^))

-- The problem

triangles :: Integral a => [a]
triangles = fmap fst $ iterate (uncurry (+) &&& (+1) . snd) (0, 1)

nfactors :: Integral a => a -> Integer
nfactors = mcombinations . factorize

solve :: Integer -> Maybe Integer
solve n = find ((> n) . nfactors) triangles

solution = maybe undefined id (solve 500)
