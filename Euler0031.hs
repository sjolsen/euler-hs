module Euler0031 where

combineCoins :: Integral a => [a] -> a -> a
combineCoins [] _ = 0
combineCoins (x:xs) n = case compare n 0 of
  LT -> 0
  EQ -> 1
  GT -> combineCoins (x:xs) (n-x) + combineCoins xs n

solution :: IO Integer
solution = return (combineCoins [200,100,50,20,10,5,2,1] 200)
