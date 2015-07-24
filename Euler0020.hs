module Euler0020 where

import Common.Combinatorics
import Data.Digits

solution :: IO Integer
solution = return . sum . digits 10 . factorial $ 100
