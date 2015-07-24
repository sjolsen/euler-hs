module Euler0016 where

import Data.Digits

solution = sum . digits 10 $ 2^1000
