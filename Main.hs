module Main (main) where
import System.Random (mkStdGen)
import Primes (generatePrime)


main :: IO ()
main = print $ generatePrime 1024 (mkStdGen 42)
