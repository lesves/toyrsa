{-
This module contains functions for primality tests 
and prime number generation. 

It is used by the key generator.
-}
module Lib.Primes (isPrime, generatePrime) where

import System.Random (RandomGen, randomR)
import Lib.Utils (modExp)


-- generate an integer with the given number of bits
randBigInteger :: (RandomGen g) => Int -> g -> (Integer, g)
randBigInteger bits = randomR (0, 2^bits - 1)

data PrimalityStatus = ProbablyPrime | Composite deriving (Eq, Show)

-- perform the Miller-Rabin primality test of the number n with k iterations
millerRabin :: (RandomGen g) => Integer -> Int -> g -> (PrimalityStatus, g)
millerRabin n k rand = loop k rand where
    loop 0 rand = (ProbablyPrime, rand)
    loop k rand = case millerRabinOnce n s d rand of
        c@(Composite, rand') -> c
        (ProbablyPrime, rand') -> loop (k-1) rand'

    (d, s) = factor (n-1) 0
    factor d s = if even d
        then factor (d `div` 2) (s+1)
        else (d, s)

-- do a single iteration of the Miller-Rabin primality test (a single base)
millerRabinOnce :: (RandomGen g) => Integer -> Int -> Integer -> g -> (PrimalityStatus, g)
millerRabinOnce n s d rand = (
        iter (modExp base d n) s,
        rand'
    ) where
        iter x 0 = if x /= 1 then Composite else ProbablyPrime -- Fermat's witness
        iter x it = let y = x*x `rem` n in if y == 1 && x /= 1 && x /= n-1
            then Composite
            else iter y (it-1)
        (base, rand') = randomR (2, n-2) rand

-- check if a number is prime using the Miller-Rabin primality test
-- with 10 iterations, the chance of failure is at most 4^-10
isPrime :: (RandomGen g) => Integer -> g -> (Bool, g)
isPrime n rand = if even n
        then (False, rand)
        else (res == ProbablyPrime, rand')
    where
        (res, rand') = millerRabin n 10 rand

-- generate a prime by generating random numbers until one is prime
generatePrime :: (RandomGen g) => Int -> g -> (Integer, g)
generatePrime bits rand = if result
        then (candidate, rand'')
        else generatePrime bits rand''
    where
        (result, rand'') = isPrime candidate rand'
        (candidate, rand') = randBigInteger bits rand
