module Common.List where

import Data.List

-- Limits for Ord a => [a]

whileSatisfying :: [a] -> (a -> Bool) -> [a]
whileSatisfying []     p = []
whileSatisfying (x:xs) p
  | p x       = x : (xs `whileSatisfying` p)
  | otherwise = []

upTo :: Ord a => [a] -> a -> [a]
upTo l n = l `whileSatisfying` (<= n)

downTo :: Ord a => [a] -> a -> [a]
downTo l n = l `whileSatisfying` (>= n)

below :: Ord a => [a] -> a -> [a]
below l n = l `whileSatisfying` (< n)

above :: Ord a => [a] -> a -> [a]
above l n = l `whileSatisfying` (> n)

uponSatisfying :: [a] -> (a -> Bool) -> [a]
uponSatisfying []     p = []
uponSatisfying (x:xs) p
  | p x       = (x:xs)
  | otherwise = uponSatisfying xs p

from :: Eq a => [a] -> a -> [a]
from l x = l `uponSatisfying` (== x)

upTo' :: Eq a => [a] -> a -> [a]
upTo' []     y = []
upTo' (x:xs) y
  | x == y    = [x]
  | otherwise = x : xs `upTo'` y

-- Subsequences

takeMaybe :: Int -> [a] -> Maybe [a]
takeMaybe n l =
  let j = take n l in
  if length j == n
    then Just j
    else Nothing

adjacentSubsequences :: Int -> [a] -> [[a]]
adjacentSubsequences 0 _      = [[]]
adjacentSubsequences _ []     = []
adjacentSubsequences n (x:xs) = case takeMaybe n (x:xs) of
  Just l  -> l : adjacentSubsequences n xs
  Nothing -> []

-- Strict maximumBy

maximumBy' :: (a -> a -> Ordering) -> [a] -> a
maximumBy' cmp (x:xs) = foldl' (maxBy cmp) x xs
  where maxBy cmp a b = case cmp a b of
          LT -> b
          EQ -> a
          GT -> a

-- Counting

count :: Integral i => (a -> Bool) -> [a] -> i
count p xs = go 0 p xs
  where
    go acc p [] = acc
    go acc p (x:xs)
      | p x = go (1 + acc) p xs
      | otherwise = go acc p xs

-- Uniqueness

-- Assumes partitioned input
uniq :: Eq a => [a] -> [a]
uniq []  = []
uniq [x] = [x]
uniq (x:y:z)
  | x == y    =     uniq (y:z)
  | otherwise = x : uniq (y:z)

unique :: Ord a => [a] -> [a]
unique = uniq . sort

-- Assumes strictly sorted input
setMinus :: Ord a => [a] -> [a] -> [a]
setMinus []     _  = []
setMinus x      [] = x
setMinus (x:xs) (y:ys) = case compare x y of
  LT -> x : (xs `setMinus` (y:ys))
  EQ -> xs `setMinus` ys
  GT -> (x:xs) `setMinus` ys
