import System.Environment (getArgs)
import qualified Data.ByteString.Lazy.Char8 as CH8
import qualified Data.ByteString.Base16.Lazy as B16
import Data.Crypto.ByteString
import Data.Crypto.English
import Data.Crypto.Frequency


main :: IO ()
main = do
    bs <- fmap (fst . B16.decode . CH8.pack . head) getArgs

    -- all letters can be keys.
    let keys = map (CH8.pack . flip (:) []) [' '..'~']

    -- apply xor with each key
    let xoredStrings = filter validEnglish $ map (CH8.unpack . flip xorBS bs) keys

    let chi2s = map (\s -> (charChi2 engFreqMap s, s)) xoredStrings

    print . minimum $ chi2s
