module Euler0001 where

import Common.Math

solution = sum [x | x <- [1..999], 3 `divides` x || 5 `divides` x]
