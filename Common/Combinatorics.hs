module Common.Combinatorics where

import Data.Bifunctor
import Data.List

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

-- Produces each way of selecting n elements, paired with the remainder
select :: Integral n => n -> [a] -> [([a],[a])]
select 0 xs = [([], xs)]
select n xs = do
  (y, ys)  <- selects xs
  (as, bs) <- select (n - 1) ys
  return (y:as, bs)

-- Produces the permutations of the input in lexicographical order, based on the
-- order of elements of the input
lexpermute :: [a] -> [[a]]
lexpermute [] = [[]]
lexpermute s  = [x : xs' | (x,xs) <- selects s
                         , xs' <- lexpermute xs]

isPermutation :: Ord a => [a] -> [a] -> Bool
isPermutation a b = sort a == sort b
