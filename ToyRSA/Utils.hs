module ToyRSA.Utils (modExp, inv) where

-- exponentiation modulo m
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
    alg t t' r r' = if r' == 0
        then if r > 1
            then Nothing
            else Just ((t+n) `rem` n)
        else let q = r `div` r' in alg t' (t - q*t') r' (r - q*r')
