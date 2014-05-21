module Base16 where

import WordN
import qualified Data.Map as M
import Data.Tuple (swap)
import Data.Word (Word8)
import Data.List.Split (chunksOf)
import qualified Data.Vector as V (fromList, toList, concat)

toChar :: WordN -> Char
toChar w = toMap M.! toBits w

fromChar :: Char -> WordN
fromChar c = fromBits 4 $ fromMap M.! c

unpack :: WordN -> [WordN]
unpack = reverse . map V.fromList . chunksOf 4 . V.toList

pack :: [WordN] -> WordN
pack = V.concat . reverse

-- internal

fromMap :: M.Map Char Word8
fromMap = M.fromList encoding

toMap :: M.Map Word8 Char
toMap = M.fromList (map swap encoding)

encoding :: [(Char, Word8)]
encoding = 
    [('0', 0), ('1', 1), ('2', 2), ('3', 3),
     ('4', 4), ('5', 5), ('6', 6), ('7', 7),
     ('8', 8), ('9', 9), ('A', 10), ('B', 11),
     ('C', 12), ('D', 13), ('E', 14), ('F', 15),

     ('a', 10), ('b', 11), ('c', 12), ('d', 13),
     ('e', 14), ('f', 15)]
