module Euler0015 where

import Common.Combinatorics

data Direction = DRight | DDown

solution = mpermutations [(DRight, 20), (DDown, 20)]
