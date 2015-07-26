module Euler0023 where

import Common.Factorize
import Common.List
import Data.List

abundant :: Integral a => a -> Bool
abundant n = sum (pdivisors n) > n

limit :: Integral a => a
limit = 28123

abundantNumbers :: Integral a => [a]
abundantNumbers = filter abundant [1..]

abundantSums :: Integral a => [a]
abundantSums = unique [a+b | a <- abundantNumbers `upTo` limit
                           , b <- abundantNumbers `from` a `upTo` (limit - a)]

solution :: IO Integer
solution = return . sum $ [1..28123] `setMinus` abundantSums
