import Crypto
import Base16 as B16
import ASCII as ASCII
import English
import Data.List.Extras (argmin)
import Control.Applicative ((<$>), (<*>))


main :: IO ()
main = do
    let toWordN = return . B16.pack . map B16.fromChar
    dat <- toWordN =<< getLine

    let keys = map ASCII.fromChar alphabet

    let xord = fmap (flip xorKey dat) keys
    let strs = filter valid $ (map ASCII.toChar . ASCII.unpack) <$> xord

    print $ argmin englCharChi2 strs
