module Euler0029 where

import Common.List
import Data.List

solution :: IO Integer
solution = return . toInteger . length . unique
         $ [a^b | a <- [2..100], b <- [2..100]]
