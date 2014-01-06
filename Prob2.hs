import Crypto
import Convert

main :: IO ()
main = do
    a <- getLine
    b <- getLine

    putStrLn $ boolsToStr16 $ zipWith xor (strToBools16 a) (strToBools16 b)
