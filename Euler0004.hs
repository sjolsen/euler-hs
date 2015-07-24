module Euler0004 where

isPalindromic :: Show a => a -> Bool
isPalindromic a = let s = show a in s == reverse s

solution :: IO Integer
solution = return . maximum $ filter isPalindromic [a*b | a <- [100..999], b <- [100..999]]
