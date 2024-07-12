{-
This module contains the basic functions
for implementing the RSA cryptosystem.
-}
module Lib.Utils (modExp, inv) where

-- exponentiation modulo m
-- (implemented as exponentiation by squaring)
modExp :: Integer -> Integer -> Integer -> Integer
modExp b 0 m = 1
modExp b e m
  | e' `rem` 2 == 1 = b'*modExp square ((e-1) `div` 2) m `rem` m
  | otherwise = modExp square (e `div` 2) m `rem` m
  where
    b' = b `rem` m
    e' = e `rem` m
    square = b'*b'

-- multiplicative inverse
inv :: Integer -> Integer -> Maybe Integer
inv a n = alg 0 1 n a where
    -- we use a modified extended Euclidean algorithm
    alg t t' r r' = if r' == 0
        then if r > 1
            -- n and a have gcd > 1, which means a doesn't have an inverse
            then Nothing
            else Just ((t+n) `rem` n) -- t may be negative (but >= -n)
        else let q = r `div` r' in alg t' (t - q*t') r' (r - q*r')
