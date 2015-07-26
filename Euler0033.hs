module Euler0033 where

import Common.Combinatorics
import Common.Factorize
import Common.Math
import Common.List
import Data.List
import Data.Digits

data Rational' = Div {num :: Integer, den :: Integer}

repEq :: Rational' -> Rational' -> Bool
repEq (Div a b) (Div c d) = a == c && b == d

instance Eq Rational' where
  p == q = repEq (canonicalize p) (canonicalize q)

instance Show Rational' where
  show (Div n d) = show n ++ "/" ++ show d

mult :: Rational' -> Rational' -> Rational'
mult p q = Div (num p * num q) (den p * den q)


simplifyWith :: (Integer -> Multiset Integer) -> (Multiset Integer -> Integer)
             -> Rational' -> Rational'
simplifyWith fact unfact (Div num den) =
  let nums  = fact num
      dens  = fact den
      comms = commonElts nums dens
      num'  = unfact $ nums `multisetMinus` comms
      den'  = unfact $ dens `multisetMinus` comms
  in Div num' den'

canonicalize :: Rational' -> Rational'
canonicalize = simplifyWith factorize unfactorize

simplify :: Rational' -> Rational'
simplify p@(Div num den) =
  let [a,b] = digits 10 num
      [c,d] = digits 10 den
  in
   if      (a == c) then (Div b d)
   else if (a == d) then (Div b c)
   else if (b == c) then (Div a d)
   else if (b == d) then (Div a c)
   else p

trivial :: Rational' -> Bool
trivial p@(Div n d) = (10 `divides` n && 10 `divides` d) || simplify p `repEq` p

solution :: IO Integer
solution =
  let fracs = [f | b <- [10..99]
                 , a <- [10..b-1]
                 , let f = Div a b
                 , not (trivial f)
                 , f == simplify f]
      prod  = foldl' mult (Div 1 1) fracs
  in return . den . canonicalize $ prod
