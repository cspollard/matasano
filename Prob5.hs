import Crypto
import qualified ASCII
import qualified Base16 as B16
import qualified Data.Vector as V

main :: IO ()
main = do
    let f = ASCII.pack . map ASCII.fromChar
    let s = f "Burning 'em, if you ain't quick and nimble\nI go crazy when I hear a cymbal"

    -- are the keys supposed to be applied one at a time or all at
    -- once?!
    let ks = map f ["I", "C", "E"]
    print . map B16.toChar . B16.unpack $ foldr xorKey s ks
