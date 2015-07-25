module Euler0025 where

import Common.Fibonacci
import Data.List

solution :: IO Integer
solution = return . maybe 0 toInteger . findIndex (>= (10^999)) $ fibs
