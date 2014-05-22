import Crypto
import Data.List.Extras (argmin)
import Data.List (sort)
import qualified ASCII as ASCII
import qualified Base16 as B16
import English
import Control.Monad (liftM)
import Control.Arrow ((&&&))
import System.Environment (getArgs)
import WordN (fromBits)

main :: IO ()
main = do
    bs <- liftM lines . readFile =<< return . head . tail =<< getArgs

    let instrings = map (B16.pack . map B16.fromChar) bs
    let keys = map (fromBits 8) ([32..126] :: [Int])

    let outstrings = filter valid [(map ASCII.toChar . ASCII.unpack) (k `xorKey` s) | k <- keys, s <- instrings]

    dict <- loadDict =<< return . head =<< getArgs

    case dict of
        Left s -> print s
        Right d -> print $ reverse . sort $ map (charChi2 d &&& id) outstrings
        -- Right d -> print $ argmin (charChi2 d) outstrings

    -- let beststrings = reverse . sort $ map (englCharChi2 &&& id) outstrings
    -- print beststrings
