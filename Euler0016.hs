module Euler0016 where

import Data.Char

solution = toInteger . sum . fmap digitToInt . show $ 2^1000
