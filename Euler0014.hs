module Euler0014 where

import Common.List
import Common.Math
import Data.Ord
import Data.List
import Control.Arrow

collatzStep :: Integral a => a -> a
collatzStep n
  | even n    = n `div` 2
  | otherwise = 3*n + 1

collatzLength :: Integral a => a -> a
collatzLength 1 = 1
collatzLength n = 1 + collatzLength (collatzStep n)

solve :: Integer -> Integer
solve n = fst . maximumBy' (comparing snd) . fmap (id &&& collatzLength) $ natPlus `below` n

solution :: IO Integer
solution = return $ solve 1000000
