module Main (main) where
import System.Environment (getArgs)
import System.Random (getStdGen)
import ToyRSA.RSA (generateKeys, encrypt, decrypt)


help :: IO ()
help = do
    putStrLn "==== ToyRSA ===="
    putStrLn "Commands:       "
    putStrLn "--gen <keyname> "
    putStrLn "--enc <keyname> "
    putStrLn "--dec <keyname> "


genKeyAndSave :: String -> Int -> IO ()
genKeyAndSave keyname bits = do
    rand <- getStdGen
    let ((pub, priv), _) = generateKeys 1024 rand
    writeFile (keyname ++ ".pub") (show pub)
    writeFile keyname (show priv)


stringDump :: String -> Integer
stringDump str = foldl (\a x -> a*base + x) 0 (map (toInteger . fromEnum) str) where
    base = toInteger $ 1 + fromEnum (maxBound :: Char)


stringLoad :: Integer -> String
stringLoad enc = reverse $ convert enc where
    convert 0 = ""
    convert enc = toEnum (fromIntegral $ enc `rem` base):convert (enc `div` base)
    base = toInteger $ 1 + fromEnum (maxBound :: Char)


loadKeyAndEncryptFromStdin :: String -> IO ()
loadKeyAndEncryptFromStdin keyname = do
    pubkey <- read <$> readFile (keyname ++ ".pub")
    putStrLn "key found"
    putStrLn "enter text to be encrypted; then end input using ^D: "
    plaintext <- getContents
    let m = stringDump plaintext
    let c = encrypt pubkey m
    putStrLn $ replicate 80 '='
    putStrLn $ show c


loadKeyAndDecryptFromStdin :: String -> IO ()
loadKeyAndDecryptFromStdin keyname = do
    privkey <- read <$> readFile keyname
    putStrLn "key found"
    putStrLn "paste encrypted text on the next line: "
    ciphertext <- read <$> getLine
    let m = decrypt privkey ciphertext
    putStrLn $ replicate 80 '='
    putStrLn $ stringLoad m

main :: IO ()
main = do
    args <- getArgs
    case args of
        ["--gen", keyname] -> 
            putStrLn "info: key size not specified, default is 1024" >> 
            genKeyAndSave keyname 1024
        ["--gen", keyname, bits] -> genKeyAndSave keyname (read bits)
        ["--enc", keyname] -> loadKeyAndEncryptFromStdin keyname
        ["--dec", keyname] -> loadKeyAndDecryptFromStdin keyname
        _ -> help
