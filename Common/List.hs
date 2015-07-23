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
