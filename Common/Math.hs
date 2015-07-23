module Common.Math where

-- Basic integer math

isqrt :: Integral a => a -> a
isqrt = floor . sqrt . fromIntegral

divides :: Integral a => a -> a -> Bool
divides a b = (b `mod` a) == 0
