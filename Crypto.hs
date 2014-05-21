module Crypto where

import English
import Data.List.Extras (argmin, argminWithMin)
import Data.List.Split (chunksOf)
import WordN
import qualified Data.Vector as V (fromList, toList, length)
import Data.Bits (xor)


xorKey :: WordN -> WordN -> WordN
xorKey key dat = (V.fromList . take (V.length dat) . cycle . V.toList) key `xor` dat

alphabet :: String
alphabet = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"
