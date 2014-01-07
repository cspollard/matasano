import Crypto
import Convert
import Frequency
import Data.List.Extras (argmin, argminWithMin)


main :: IO ()
main = do
    a <- getLine

    print $ argminWithMin (englCharChi2 . boolsToStrASCII . xors (strToBools16 a) . charToBoolsASCII) "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"
    -- print $ map (englCharChi2 . boolsToStrASCII . xors (strToBools16 a) . charToBoolsASCII) "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"

    return ()
