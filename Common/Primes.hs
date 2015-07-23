{-
  The following implementation is due to O'Neill (The Genuine Sieve
  of Eratosthenes).
-}

module Common.Primes where

import qualified Common.Primes.PriorityQ as PQ

sieve :: Integral a => [a] -> [a]
sieve (x:xs) = x : sieve' xs (insertprime x xs PQ.empty)
  where
    insertprime p xs table = PQ.insert (p*p) (map (* p) xs) table
    sieve' []     table = []
    sieve' (x:xs) table
      | nextComposite <= x  = sieve' xs (adjust table)
      | otherwise           = x : sieve' xs (insertprime x xs table)
        where
          nextComposite = PQ.minKey table
          adjust table
            | n <= x    = adjust (PQ.deleteMinAndInsert n' ns table)
            | otherwise = table
              where
                (n, n':ns) = PQ.minKeyValue table

wheel2357 :: Integral a => [a]
wheel2357 = 2:4:2:4:6:2:6:4:2:4:6:6:2:6:4:2:6:4:6:8:4:2:4:2:4:8
           :6:4:6:2:4:6:2:6:6:4:2:4:6:2:6:4:2:4:2:10:2:10:wheel2357

spin :: Integral a => [a] -> a -> [a]
spin (x:xs) n = n : spin xs (n + x)

primes :: Integral a => [a]
primes = 2 : 3 : 5 : 7 : sieve (spin wheel2357 11)
