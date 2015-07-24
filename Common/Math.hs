module Common.Math where

nats :: Integral a => [a]
nats = [0..]

natPlus :: Integral a => [a]
natPlus = [1..]

-- Basic integer math

isqrt :: Integral a => a -> a
isqrt = floor . sqrt . fromIntegral

divides :: Integral a => a -> a -> Bool
divides a b = (b `mod` a) == 0

-- Logic

implies :: Bool -> Bool -> Bool
implies True False = False
implies _    _     = True
