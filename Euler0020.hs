module Euler0020 where

import Common.Combinatorics
import Data.Digits

solution = sum . digits 10 . factorial $ 100
