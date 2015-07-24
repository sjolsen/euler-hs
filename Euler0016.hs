module Euler0016 where

import Data.Digits

solution :: IO Integer
solution = return . sum . digits 10 $ 2^1000
