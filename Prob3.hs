import Crypto
import qualified Base16 as B16
import qualified ASCII
import English
import System.Environment (getArgs)
import Data.List (sortBy)
import Control.Arrow ((&&&))


main :: IO ()
main = do
    let toWordN = return . B16.pack . map B16.fromChar
    dat <- toWordN =<< getLine

    let keys = map ASCII.fromChar [' '..'~']

    let xord = map (`xorKey` dat) keys
    let strs = filter valid $ map (map ASCII.toChar . ASCII.unpack) xord

    dict <- loadDict =<< return . head =<< getArgs

    case dict of
        Left s -> print s
        Right d -> print $ sortBy (flip compare) $ map (charChi2 d &&& id) strs
