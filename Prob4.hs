import System.Environment (getArgs)
import qualified Data.ByteString.Base16.Lazy as B16
import qualified Data.ByteString.Lazy.Char8 as CH8
import Data.List (sort)

import Data.Crypto.English
import Data.Crypto.Frequency
import Data.Crypto.ByteString


main :: IO ()
main = do
    input <- CH8.readFile =<< fmap head getArgs
    let bss = map B16.encode $ CH8.lines input

    let keys = map (CH8.pack . flip (:) []) [' '..'~']

    let xoredStrings = filter validEnglish . map CH8.unpack $ [xorBS k bs | k <- keys, bs <- bss]
    
    print xoredStrings

    let chi2s = map (\s -> (charChi2 engFreqMap s, s)) xoredStrings

    print . take 20 . sort $ chi2s

    {-
    let instrings = map (B16.pack . map B16.fromChar) bs

    let keys = map ASCII.fromChar [' '..'~']

    let outstrings = filter valid [(map ASCII.toChar . ASCII.unpack) (k `xorKey` s) | k <- keys, s <- instrings]

    dict <- loadDict =<< return . head =<< getArgs

    case dict of
        Left s -> print s
        Right d -> mapM_ print $ (sortBy (flip compare) . map (charChi2 d &&& id)) outstrings

    -- let beststrings = reverse . sort $ map (englCharChi2 &&& id) outstrings
    -- print beststrings
    -}
