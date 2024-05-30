module Lib.RSA (PublicKey, PrivateKey, generateKeys, encrypt, decrypt) where
import System.Random (RandomGen)
import Lib.Primes (generatePrime)
import Lib.Utils (modExp, inv)


data PublicKey = PublicKey { pubN :: Integer, pubE :: Integer } deriving (Show, Read, Eq)
data PrivateKey = PrivateKey { privN :: Integer, privD :: Integer } deriving (Show, Read, Eq)

generateKeys :: (RandomGen g) => Int -> g -> ((PublicKey, PrivateKey), g)
generateKeys bits rand = if valid 
        then ((PublicKey { pubN = n, pubE = e }, PrivateKey { privN = n, privD = d }), rand'')
        else generateKeys bits rand''
    where
        (p, rand') = generatePrime (bits `div` 2) rand
        (q, rand'') = generatePrime (bits `div` 2) rand'
        n = p*q
        l_n = lcm (p-1) (q-1)
        e = 2^16 + 1
        d = case inv e l_n of
            Nothing -> error "invalid state"
            Just x -> x

        -- e must not be a divisor of l_n
        valid = l_n `rem` e /= 0

encrypt :: PublicKey -> Integer -> Integer
encrypt key m = modExp m (pubE key) (pubN key)

decrypt :: PrivateKey -> Integer -> Integer
decrypt key c = modExp c (privD key) (privN key)
