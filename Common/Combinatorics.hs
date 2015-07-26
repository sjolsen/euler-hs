module Common.Combinatorics where

import Data.Bifunctor
import Data.List
import Data.Function
import Common.List

type Multiset a = [(a, Integer)]

factorial :: Integral a => a -> a
factorial n = product [1..n]

toMultiset :: Eq a => [a] -> Multiset a
toMultiset []     = []
toMultiset (x:xs) =
  let (eq, neq) = partition (== x) xs
      xcount    = toInteger $ 1 + length eq
  in (x,xcount) : toMultiset neq

fromMultiset :: Multiset a -> [a]
fromMultiset []         = []
fromMultiset ((x,n):xs) = replicate (fromIntegral n) x ++ fromMultiset xs

commonElts :: Integral a => Multiset a -> Multiset a -> Multiset a
commonElts a b =
  let elts    = fmap fst $ intersectBy ((==) `on` fst) a b
      acoeffs = fmap (`assoc` a) elts
      bcoeffs = fmap (`assoc` b) elts
      common  = fmap (uncurry min) $ zipWith (,) acoeffs bcoeffs
  in zipWith (,) elts common

multisetMinus :: Ord a => Multiset a -> Multiset a -> Multiset a
multisetMinus a b = multisetMinus' (sort a) (sort b)
  where
    multisetMinus' []         y          = fmap (bimap id negate) y
    multisetMinus' x          []         = x
    multisetMinus' ((x,m):xs) ((y,n):ys) = case compare x y of
      LT -> (x,m) : (xs `multisetMinus` ((y,n):ys))
      EQ -> if m == n
            then xs `multisetMinus` ys
            else (x,m-n) : (xs `multisetMinus` ys)
      GT -> ((x,m):xs) `setMinus` ys

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
