module Main (main) where
import System.Random (getStdGen)
import RSA (generateKeys, encrypt, decrypt)


main :: IO ()
main = do
    rand <- getStdGen
    let ((pub, priv), rand') = generateKeys 1024 rand
    let m = 1729
    print m
    let c = encrypt pub m
    print c
    let m' = decrypt priv c
    print m'
