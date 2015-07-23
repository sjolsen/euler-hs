module Common.Combinatorics where

type Multiset a = [(a, Integer)]

mcombinations :: Multiset a -> Integer
mcombinations = product . fmap ((+1) . snd)
