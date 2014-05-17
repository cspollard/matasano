module Hex where

-- UGGGH. Need to deal with bit packing (16 = 4 bytes).

import qualified Data.ByteString.Lazy as BS
import Data.ByteString.Lazy (ByteString)
import qualified Data.Map as M
import Data.Tuple (swap)
import Data.Word

fromHex :: String -> ByteString
fromHex = BS.pack . map w8FromHex

toHex :: ByteString -> String
toHex = map w8ToHex . BS.unpack

w8ToHex :: Word8 -> Char
w8ToHex w = toHexMap M.! w

w8FromHex :: Char -> Word8
w8FromHex c = fromHexMap M.! c

fromHexMap :: M.Map Char Word8
fromHexMap = M.fromList encodeHex

toHexMap :: M.Map Word8 Char
toHexMap = M.fromList (map swap encodeHex)

encodeHex :: [(Char, Word8)]
encodeHex = 
    [('0', 0),
     ('1', 1),
     ('2', 2),
     ('3', 3),
     ('4', 4),
     ('5', 5),
     ('6', 6),
     ('7', 7),
     ('8', 8),
     ('9', 9),

     ('A', 10),
     ('B', 11),
     ('C', 12),
     ('D', 13),
     ('E', 14),
     ('F', 15),

     ('a', 10),
     ('b', 11),
     ('c', 12),
     ('d', 13),
     ('e', 14),
     ('f', 15)]
