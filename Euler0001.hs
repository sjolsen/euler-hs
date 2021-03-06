module Euler0001 where

import Common.Math
import Common.List

solution :: IO Integer
solution = return $ sum [x | x <- natPlus `below` 1000, 3 `divides` x || 5 `divides` x]
