module Euler0023 where

import Common.Factorize
import Common.List
import Data.List

abundant :: Integral a => a -> Bool
abundant n = sum (pdivisors n) > n

abundantNumbers :: Integral a => [a]
abundantNumbers = filter abundant [1..]

abundantSums :: Integral a => [a]
abundantSums = uniq $ sort [a+b | a <- abundantNumbers `upTo` 28123
                                , b <- abundantNumbers `upTo` (28123 - a)]

solution :: IO Integer
solution = return . sum $ [1..28123] `setMinus` abundantSums
