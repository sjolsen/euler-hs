module Euler0002 where

import Common.List
import Common.Fibonacci

solution = sum $ filter even $ fibs `upTo` 4000000
