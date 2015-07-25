module Euler0024 where

import Common.Combinatorics
import Data.Digits

solution :: IO Integer
solution = return . unDigits 10 $ lexpermute [0..9] !! (1000000-1)
