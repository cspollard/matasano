module Base64 where

-- UGGGH. Need to deal with bit packing (64 = 6 bytes).

import qualified Data.ByteString.Lazy as BS
import Data.ByteString.Lazy (ByteString)
import qualified Data.Map as M
import Data.Tuple (swap)
import Data.Word

fromBase64 :: String -> ByteString
fromBase64 = BS.pack . map w8FromBase64

toBase64 :: ByteString -> String
toBase64 = map w8ToBase64 . BS.unpack

w8ToBase64 :: Word8 -> Char
w8ToBase64 w = toBase64Map M.! w

w8FromBase64 :: Char -> Word8
w8FromBase64 c = fromBase64Map M.! c

fromBase64Map :: M.Map Char Word8
fromBase64Map = M.fromList encodeBase64

toBase64Map :: M.Map Word8 Char
toBase64Map = M.fromList (map swap encodeBase64)

encodeBase64 :: [(Char, Word8)]
encodeBase64 = 
    [('A', 0), ('B', 1), ('C', 2), ('D', 3),
     ('E', 4), ('F', 5), ('G', 6), ('H', 7),
     ('I', 8), ('J', 9), ('K', 10), ('L', 11),
     ('M', 12), ('N', 13), ('O', 14), ('P', 15),
     ('Q', 16), ('R', 17), ('S', 18), ('T', 19),
     ('U', 20), ('V', 21), ('W', 22), ('X', 23),
     ('Y', 24), ('Z', 25), ('a', 26), ('b', 27),
     ('c', 28), ('d', 29), ('e', 30), ('f', 31),
     ('g', 32), ('h', 33), ('i', 34), ('j', 35),
     ('k', 36), ('l', 37), ('m', 38), ('n', 39),
     ('o', 40), ('p', 41), ('q', 42), ('r', 43),
     ('s', 44), ('t', 45), ('u', 46), ('v', 47),
     ('w', 48), ('x', 49), ('y', 50), ('z', 51),
     ('0', 52), ('1', 53), ('2', 54), ('3', 55),
     ('4', 56), ('5', 57), ('6', 58), ('7', 59),
     ('8', 60), ('9', 61), ('+', 62), ('/', 63)]
