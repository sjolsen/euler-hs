module Euler0009 where

triples :: Integral a => [(a,a,a)]
triples = do
  a <- [0..1000]
  b <- [a..1000-a]
  let c = 1000 - a - b
  if c > b && a^2 + b^2 == c^2
    then return (a,b,c)
    else []

solution = let (a,b,c) = triples !! 0 in a*b*c
