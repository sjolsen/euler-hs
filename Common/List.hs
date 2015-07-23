module Common.List where

-- Limits for Ord a => [a]

whileSatisfying :: [a] -> (a -> Bool) -> [a]
whileSatisfying []     p = []
whileSatisfying (x:xs) p
  | p x       = x : (xs `whileSatisfying` p)
  | otherwise = []

upTo :: Ord a => [a] -> a -> [a]
upTo l n = l `whileSatisfying` (<= n)

below :: Ord a => [a] -> a -> [a]
below l n = l `whileSatisfying` (< n)

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
