module Euler0005 where

import Data.List

solution = foldl' lcm 1 [1..20]
