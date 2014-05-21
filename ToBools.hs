{-# LANGUAGE FlexibleInstances #-}

module ToBools where

import Data.ByteString (ByteString, unpack, pack)
import Data.Word (Word8)
import Data.Bits
import Data.List (foldl')
import Data.List.Split (chunksOf)


setBitBool :: Bits a => a -> Int -> Bool -> a
setBitBool x i b = if b
    then x `setBit` i
    else x `clearBit` i

class ToBools b where
    toBools :: b -> [Bool]

class FromBools b where
    fromBools :: [Bool] -> b

w8FromBools :: [Bool] -> Word8
w8FromBools bs = foldl' (\w (b, i) -> setBitBool w i b) 0 $ zip (reverse bs) [0..7]

w8ToBools :: Word8 -> [Bool]
w8ToBools w = map (testBit w) [7, 6, 5, 4, 3, 2, 1, 0]

instance FromBools [Word8] where
    fromBools bs = map w8FromBools $ (reverse . chunksOf 8 . reverse) bs

instance ToBools [Word8] where
    toBools = concatMap w8ToBools

instance ToBools ByteString where
    toBools = toBools . unpack

instance FromBools ByteString where
    fromBools = pack . fromBools
