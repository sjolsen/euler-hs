module Common.Combinatorics where

import Data.Bifunctor

type Multiset a = [(a, Integer)]

factorial :: Integral a => a -> a
factorial n = product [1..n]

-- Multisets

mcombinations :: Multiset a -> Integer
mcombinations = product . fmap ((+1) . snd)

mpermutations :: Multiset a -> Integer
mpermutations s = let ms = fmap snd s
                      n  = sum ms
                  in factorial n `div` product (fmap factorial ms)

mcombine :: Multiset a -> [Multiset a]
mcombine []         = [[]]
mcombine ((x,n):xs) =
  let cs = mcombine xs
  in cs ++ [(x,m):c | m <- [1..n], c <- cs]

-- Lexicographical permutation

-- Produces each element paired with the rest of the list, in order
selects :: [a] -> [(a,[a])]
selects []     = []
selects (x:xs) = (x,xs) : fmap (bimap id (x:)) (selects xs)

-- Produces the permutations of the input in lexicographical order, based on the
-- order of elements of the input
lexpermute :: [a] -> [[a]]
lexpermute [] = [[]]
lexpermute s  = [x : xs' | (x,xs) <- selects s
                         , xs' <- lexpermute xs]
