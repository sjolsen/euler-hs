module Euler0032 where

import Common.Combinatorics
import Common.List
import Data.Digits
import Data.List

ndigits :: Integral a => a -> a
ndigits = fromIntegral . length . digits 10

repNum :: Integral a => a -> a -> a
repNum d n = unDigits 10 (take (fromIntegral n) $ repeat d)

cuts :: Integral a => [(a,a)]
cuts = [(a,b) | a <- [1..9]
              , b <- [a..9-a]
              , ndigits ((1 `repNum` a) * (1 `repNum` b)) <= 9 - (a+b)
              , ndigits ((9 `repNum` a) * (9 `repNum` b)) >= 9 - (a+b)]

pandigitals :: Integral a => [a]
pandigitals = unique $ do
  (la, lb)  <- cuts
  (as, as') <- select la [1..9]
  (bs, bs') <- select lb as'
  let a = unDigits 10 as
      b = unDigits 10 bs
      c = a * b
      cs = digits 10 c
  if isPermutation bs' cs
    then return c
    else []

solution :: IO Integer
solution = return (sum pandigitals)
