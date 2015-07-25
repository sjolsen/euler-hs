module Euler0027 where

{-
  Because for n = 0, n^2 + an + b = b, b must be prime.
-}

import Common.Primes
import Common.Factorize
import Common.List
import Data.Ord

isPrime :: Integral a => a -> Bool
isPrime n
  | n <= 0    = False
  | otherwise = case factorize n of
                 [(_,1)] -> True
                 _       -> False

score :: Integral a => a -> a -> a
score a b = fromIntegral . length . takeWhile isPrime $ fmap (\n -> n^2 + a*n + b) [0..]

bestAForB :: Integral a => a -> (a, a)
bestAForB b = maximumBy' (comparing snd) [(a,s) | a <- [-999..999]
                                                , let s = score a b]

solution :: IO Integer
solution = return . (\(a,b,_)->a*b) . maximumBy' (comparing (\(_,_,s)->s))
         $ [(a,b,s) | b <- primes `below` 1000
                    , let (a,s) = bestAForB b]
