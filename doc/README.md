# ToyRSA

A toy RSA implementation written in Haskell as my semestral project for a non-procedural programming course.

## Usage:
```
cabal install --lib random
make all

# Generate a key:
./Main --gen <keyfile>

# Encrypt something:
./Main --enc <keyfile>

# Decrypt something:
./Main --dec <keyfile>
```