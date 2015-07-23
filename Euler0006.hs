module Euler0006 where

sumOfSquares n = sum $ fmap (^2) [1..n]

squareOfSum  n = (^2) $ sum [1..n]

solution = abs $ sumOfSquares 100 - squareOfSum 100
