# ToyRSA

A toy RSA implementation written in Haskell as my semestral project for a non-procedural programming course.

## Usage:
```
cabal install --lib random
make all

# Generate a key:
./toyrsa --gen <keyfile>

# Encrypt something:
./toyrsa --enc <keyfile>

# Decrypt something:
./toyrsa --dec <keyfile>
```