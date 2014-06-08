import Crypto
import Data.List (sortBy)
import qualified ASCII
import qualified Base16 as B16
import English
import Control.Monad (liftM)
import Control.Arrow ((&&&))
import System.Environment (getArgs)

main :: IO ()
main = do
    bs <- liftM lines . readFile =<< return . head . tail =<< getArgs

    let instrings = map (B16.pack . map B16.fromChar) bs

    let keys = map ASCII.fromChar [' '..'~']

    let outstrings = filter valid [(map ASCII.toChar . ASCII.unpack) (k `xorKey` s) | k <- keys, s <- instrings]

    dict <- loadDict =<< return . head =<< getArgs

    case dict of
        Left s -> print s
        Right d -> mapM_ print $ (sortBy (flip compare) . map (charChi2 d &&& id)) outstrings

    -- let beststrings = reverse . sort $ map (englCharChi2 &&& id) outstrings
    -- print beststrings
