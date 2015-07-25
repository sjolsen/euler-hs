module Common.Factorize where

import Common.List
import Common.Math
import Common.Combinatorics
import Common.Primes
import Data.Bifunctor
import Data.List

factorizeWith :: Integral a => [a] -> a -> Multiset a
factorizeWith p n = factorizeWith' (p `upTo` isqrt n) n
  where
    factorizeWith' _      0 = [(0, 1)]
    factorizeWith' _      1 = []
    factorizeWith' []     n = [(n, 1)]
    factorizeWith' (p:ps) n = case n `withoutFactor` p of
      (r, 0) -> factorizeWith' ps r
      (r, m) -> (p, m) : factorizeWith' ps r

    withoutFactor n p = case n `divMod` p of
      (q, 0) -> bimap id (+1) $ q `withoutFactor` p
      _      -> (n, 0)

factorize :: Integral a => a -> Multiset a
factorize = factorizeWith primes

unfactorize :: Integral a => Multiset a -> a
unfactorize = product . fmap (uncurry (^))

divisors :: Integral a => a -> [a]
divisors = fmap unfactorize . mcombine . factorize

pdivisors :: Integral a => a -> [a]
pdivisors 0 = []
pdivisors n = delete n (divisors n)
