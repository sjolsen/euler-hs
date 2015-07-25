module Euler0030 where

import Data.Digits

{-
  No n-digit number can equal the sum of the pth powers of its digits if n*9^p
  has fewer than n digits. Because 7*9^5 = 413343 has six digits, then, it
  serves as a very loose upper bound.
-}

special :: Integral a => a -> Bool
special n = n == sum (fmap (^5) (digits 10 n))

specials :: Integral a => [a]
specials = filter special [2..413343]

solution :: IO Integer
solution = return (sum specials)
