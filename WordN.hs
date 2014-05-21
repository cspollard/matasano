{-# LANGUAGE FlexibleInstances #-}

module WordN where

import qualified Data.Vector as V
import Data.Vector (Vector, (!))

import Data.Bits
import Data.Word (Word8)

type WordN = Vector Bool

zeroBit :: Bits a => a
zeroBit = clearBit (bit 0) 0

toBits :: Bits a => WordN -> a
toBits w = V.ifoldr' (\ix x y -> if x then y .|. bit ix else y) zeroBit w

fromBits :: Bits a => Int -> a -> WordN
fromBits len i = V.generate len (testBit i)

shiftV :: Vector a -> a -> Int -> Vector a
shiftV w def n = V.generate l $
    (\ix -> let s = ix - n in
        if s >= 0 && s < l
            then w ! s
            else def
    )

    where l = V.length w

rotateV :: Vector a -> Int -> Vector a
rotateV w n = if n > l
                then rotateV w (mod n l)
                else V.generate l $
                    (\ix -> let s = (ix - n) `mod` l in w ! s)

    where l = V.length w


nand :: Bool -> Bool -> Bool
nand x = not . (&&) x

instance Bits Bool where
    (.&.) = (&&)
    (.|.) = (||)
    complement = not

    xor x y = (x `nand` y) && (x || y)

    shift b 0 = b
    shift _ _ = False

    rotate = const
    bitSize = const 1
    isSigned = const False

    bit = shift True

    testBit = shift

    popCount True = 1
    popCount False = 0

instance Bits (Vector Bool) where
    (.&.) = V.zipWith (.&.)
    (.|.) = V.zipWith (.|.)
    xor = V.zipWith xor

    complement = fmap complement

    shift = flip shiftV False
    rotate = rotateV

    bitSize = V.length
    isSigned = const False

    bit i = V.generate (i+1) (== i)
    testBit = (!)

    popCount = V.sum . V.map popCount


convertChecker :: Word8 -> Bool
convertChecker w = (toBits . fromBits 8) w == w

shiftChecker :: Word8 -> Bool
shiftChecker w = (toBits . flip shift 3 . fromBits 8) w == flip shift 3 w

rotateChecker :: Word8 -> Bool
rotateChecker w = (toBits . flip rotate 10 . fromBits 8) w == flip rotate 10 w
