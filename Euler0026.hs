module Euler0026 where

import Common.List
import Data.List
import Data.Maybe
import Data.Ord

remList :: Integral a => a -> a -> [a]
remList 0 n = []
remList m n = m : remList (10 * (m `rem` n)) n

firstRepeated :: Eq a => [a] -> Maybe a
firstRepeated l = go [] l
  where
    go seen [] = Nothing
    go seen (x:xs)
      | x `elem` seen = Just x
      | otherwise     = go (x:seen) xs

cycleLength :: Integral a => [a] -> a
cycleLength l =
  case firstRepeated l of
   Nothing -> 0
   Just x  -> let start  = fromJust . findIndex (== x) $ l
                  end'   = fromJust . findIndex (== x) $ drop (start + 1) l
                  end    = start + 1 + end'
                  length = end - start
              in fromIntegral length

rCycleLength :: Integral a => a -> a
rCycleLength r = cycleLength (1 `remList` r)

solution :: IO Integer
solution = return $ maximumBy' (comparing rCycleLength) [1..1000]
