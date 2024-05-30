module Utils (modExp) where

modExp :: Integer -> Integer -> Integer -> Integer
modExp b 0 m = 1
modExp b e m
  | e' `rem` 2 == 1 = b'*modExp square ((e-1) `div` 2) m `rem` m
  | otherwise = modExp square (e `div` 2) m `rem` m
  where
    b' = b `rem` m
    e' = e `rem` m
    square = b'*b'
