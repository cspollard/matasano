import Crypto
import qualified Base16 as B16
import qualified ASCII as ASCII
import English
import Data.List.Extras (argmin)
import Control.Applicative ((<$>))
import System.Environment (getArgs)
import Data.List (sort)
import Control.Arrow ((&&&))


main :: IO ()
main = do
    let toWordN = return . B16.pack . map B16.fromChar
    dat <- toWordN =<< getLine

    let keys = map ASCII.fromChar alphabet

    let xord = fmap (flip xorKey dat) keys
    let strs = filter valid $ (map ASCII.toChar . ASCII.unpack) <$> xord

    dict <- loadDict =<< return . head =<< getArgs

    case dict of
        Left s -> print s
        Right d -> print $ reverse . sort $ map (charChi2 d &&& id) strs
