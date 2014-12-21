import System.Environment (getArgs)
import qualified Data.ByteString.Lazy.Char8 as CH8
import qualified Data.ByteString.Base16.Lazy as B16
import qualified Data.ByteString.Lazy as BSL
import Data.Crypto.ByteString
import Data.Crypto.English
import Data.List (sort)


main :: IO ()
main = do
    bs <- fmap (fst . B16.decode . CH8.pack . head) getArgs

    -- all letters can be keys.
    let keys = map (BSL.pack . flip (:) []) [0..255]

    -- apply xor with each key
    let xoredStrings = filter validEnglish $ map (CH8.unpack . flip xorBS bs) keys

    let chi2s = map (\s -> (engChi2 s, s)) xoredStrings

    print . sort $ chi2s
