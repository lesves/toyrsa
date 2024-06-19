# ToyRSA

A toy RSA implementation written in Haskell as my semestral project for a non-procedural programming course.

## Usage:
```
cabal install --lib random
make all

# Generate a key:
./toyrsa --gen <keyfile>
# Creates <keyfile> and <keyfile>.pub

# Encrypt something:
./toyrsa --enc <keyfile>

# Decrypt something:
./toyrsa --dec <keyfile>
```

### Documentation
The program is split into 4 files. 
 - `Main.hs` contains the command line interface and padding scheme (which is *insecure*).
 - `Lib/Utils.hs` contains basic utilities for dealing with large numbers and modular arithmetic. Exponentiation modulo n (only for nonnegative exponents, using exponentiation by squaring) and multiplicative inverse (using a modified Extended Euclidean algorithm).
 - `Lib/Primes.hs` contains the algorithm which generates random primes (by generating random numbers and testing them with the Miller-Rabin primality test)
 - `Lib/RSA.hs` contains the RSA cryptosystem implementation (the most complicated part is the key generation, encryption and decryption is simple modular exponentiation)

The key format is simply a derived `Show` dump of the `PublicKey`/`PrivateKey` types from `Lib/RSA.hs`.
