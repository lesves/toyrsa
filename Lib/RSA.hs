{-
This module contains the implementation of the RSA cryptosystem.
It also defines the structure of the keys and a key generation function.
See below for an explanation of the key generation 
and a simplified proof of the correctness of the algorithm.
-}
module Lib.RSA (PublicKey, PrivateKey, generateKeys, encrypt, decrypt) where
import System.Random (RandomGen)
import Lib.Primes (generatePrime)
import Lib.Utils (modExp, inv)


-- the public key structure
data PublicKey = PublicKey { pubN :: Integer, pubE :: Integer } deriving (Show, Read, Eq)
-- the private key structure
data PrivateKey = PrivateKey { privN :: Integer, privD :: Integer } deriving (Show, Read, Eq)

-- generate the keys with a given bit size
generateKeys :: (RandomGen g) => Int -> g -> ((PublicKey, PrivateKey), g)
generateKeys bits rand = if valid 
        then ((PublicKey { pubN = n, pubE = e }, PrivateKey { privN = n, privD = d }), rand'')
        else generateKeys bits rand''
    where
        -- first, we generate two large primes, p and q
        (p, rand') = generatePrime (bits `div` 2) rand
        (q, rand'') = generatePrime (bits `div` 2) rand'

        -- we compute the modulus, n, as the multiple of p and q
        n = p*q

        -- we compute the Carmichael's totient function, l(n)
        -- we could also use lcm (p*q), which is the Euler's totient function
        -- (they are equal)
        l_n = lcm (p-1) (q-1)

        -- we choose the encryption exponent, e (it is public)
        -- we choose a smaller number for faster encryption
        e = 2^16 + 1

        -- we calculate the d as the multiplicative inverse of e modulo l_n
        d = case inv e l_n of
            Nothing -> error "invalid state"
            Just x -> x

        -- e must not be a divisor of l_n (else we restart the computation)
        valid = l_n `rem` e /= 0

-- then, encryption is exponentiation modulo n
-- to the power of the encryption exponent e
encrypt :: PublicKey -> Integer -> Integer
encrypt key m = modExp m (pubE key) (pubN key)

-- and decryption is also an exponention modulo n
-- to the power of the decryption exponent d
decrypt :: PrivateKey -> Integer -> Integer
decrypt key c = modExp c (privD key) (privN key)

-- I) (simplified) proof of correctness: 
-- (x^e)^d = x^ed = x^(phi(n)k + 1) = x^(phi(n)k) * x = 1^k * x = x

-- explanation:
-- 1) ed = 1 (mod phi(n)) => ed = phi(n)k + 1
-- 2) Euler's theorem: x^(phi(n)) = 1 (mod n)

-- II) why is it hard to break:
-- it is very hard to do the decryption (or find d) 
-- without knowing the factors of n
