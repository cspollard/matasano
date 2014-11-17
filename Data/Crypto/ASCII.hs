module ASCII where

import WordN
import Data.List.Split (chunksOf)
import qualified Data.Vector as V (fromList, toList, concat)

toChar :: WordN -> Char
toChar = toEnum . toBits

fromChar :: Char -> WordN
fromChar = fromBits 8 . fromEnum

unpack :: WordN -> [WordN]
unpack = reverse . map V.fromList . chunksOf 8 . V.toList

pack :: [WordN] -> WordN
pack = V.concat . reverse
