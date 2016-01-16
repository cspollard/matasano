import System.Environment (getArgs)
import qualified Data.ByteString.Base16.Lazy as B16
import qualified Data.ByteString.Lazy.Char8 as CH8
import qualified Data.ByteString.Lazy as BSL
import Data.List (sort)

import Data.Crypto.English
import Data.Crypto.ByteString


main :: IO ()
main = do
    input <- readFile . head =<< getArgs

    let ls = lines input

    let bss = map (fst . B16.decode . CH8.pack) ls

    -- all letters can be keys.
    let keys = map (BSL.pack . flip (:) []) [0..255]

    print keys

    let xoredStrings = map CH8.unpack [xorBS k bs | k <- keys, bs <- bss]

    print xoredStrings

    let engStrings = filter validEnglish xoredStrings

    let chi2s = sort $ map (\s -> (engChi2 s, s)) engStrings

    print chi2s

    return ()

    -- Here
    -- validEnglish and chi2 aren't working. need to rewrite.
