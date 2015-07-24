module Common.Combinatorics where

type Multiset a = [(a, Integer)]

factorial :: Integral a => a -> a
factorial n = product [1..n]

mcombinations :: Multiset a -> Integer
mcombinations = product . fmap ((+1) . snd)

mpermutations :: Multiset a -> Integer
mpermutations s = let ms = fmap snd s
                      n  = sum ms
                  in factorial n `div` product (fmap factorial ms)
