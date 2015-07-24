module Euler0002 where

import Common.List
import Common.Fibonacci

solution :: IO Integer
solution = return . sum . filter even $ fibs `upTo` 4000000
