import Crypto
import Convert
import Frequency
import Data.List.Extras (argminWithMin)


argApp :: (a -> b) -> a -> (a, b)
argApp f x = (x, f x)

main :: IO ()
main = do
    a <- getLine

    print $ map (argApp englCharChi2) $ map (boolsToStrASCII . xors (strToBools16 a) . charToBoolsASCII) "abcdefghijklmnopqrstuvwxyz"

    return ()
