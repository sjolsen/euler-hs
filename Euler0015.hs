module Euler0015 where

import Common.Combinatorics

data Direction = DRight | DDown

solution :: IO Integer
solution = return $ mpermutations [(DRight, 20), (DDown, 20)]
