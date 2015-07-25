module Euler0028 where

{-
  Begin by forming sequences a_n, b_n, c_n, and d_n like so:

    c_3  44  45 46  47  48 d_3
     42 c_2  22 23  24 d_2  26
     41  20 c_1  8 d_1  10  27
     40  19   6  1   2  11  28
     39  18 b_1  4 a_1  12  29
     38 b_2  16 15  14 a_2  30
    b_3  36  35 34  33  32 a_3

  Note that d_n = (2n+1)^2:

    43 44 45 46 47 48 49
    42 21 22 23 24 25 26
    41 20  7  8  9 10 27
    40 19  6  1  2 11 28
    39 18  5  4  3 12 29
    38 17 16 15 14 13 30
    37 36 35 34 33 32 31

  Note also that c_n = d_n - 2n, b_n = c_n - 2n, and a_n = b_n - 2n. Then,
  a_n + b_n + c_n + d_n = 4d_n - 12n = 16n^2 + 4n + 4.

  Each n sweeps out a square of size 2n+1. Thus, the solution to the problem is
  given by 1 + sum_{i=1}^500 (16n^2 + 4n + 4).

-}

solution :: IO Integer
solution = return $ 1 + sum [16*n^2 + 4*n + 4 | n <- [1..500]]
