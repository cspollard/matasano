import Base16 as B16
import Data.Bits (xor)

main :: IO ()
main = do
    let toWordN = return . B16.pack . map B16.fromChar 
    a <- toWordN =<< getLine
    b <- toWordN =<< getLine
    let c = a `xor` b

    print $ map B16.toChar . B16.unpack $ c
