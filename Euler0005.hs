module Euler0005 where

import Data.List

solution :: IO Integer
solution = return $ foldl' lcm 1 [1..20]
