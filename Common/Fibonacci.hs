module Common.Fibonacci where

import Control.Arrow

fibrecurrence :: Integral a => a -> a -> [a]
fibrecurrence a b = fmap fst $ iterate (snd &&& uncurry (+)) (a, b)

fibs :: Integral a => [a]
fibs = fibrecurrence 0 1
