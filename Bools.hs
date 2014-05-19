module Bools where

import Data.ByteString (ByteString, unpack, pack)
import Data.Word (Word8)
import Data.Bits
import Data.List (foldl', concat)
import Data.List.Split (chunksOf)

class ToBits b where
    toBits :: b -> [Bool]

instance ToBits Word8 where
    toBits w = reverse $ map (testBit w) [0..7]

class FromBits b where
    fromBits :: [Bool] -> b

instance FromBits Word8 where
    fromBits bs = foldl' (\w (b, i) -> setBitBool w i b) 0 $ zip (reverse . take 8 $ bs) [0..7]

instance ToBits ByteString where
    toBits bs = concat . map toBits $ unpack bs

instance FromBits ByteString where
    fromBits bools = pack . map fromBits $ chunksOf 8 bools

setBitBool :: Bits a => a -> Int -> Bool -> a
setBitBool x i b = if b
    then x .|. bit i
    else x `nand` bit i

nand :: Bits a => a -> a -> a
x `nand` y = x .&. complement y
