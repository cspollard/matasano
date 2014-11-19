{-# LANGUAGE FlexibleInstances #-}

module Data.Crypto.WordN where

import qualified Data.Vector as V
import Data.Vector (Vector, (!))
import Data.Bits
import Data.ByteString.Lazy


-- HERE
-- TODO
-- convert to BS


nand :: Bits b => b -> b -> b
x `nand` y = complement (x .&. y)


instance Bits (Vector Bool) where
    (.&.) = V.zipWith (.&.)
    (.|.) = V.zipWith (.|.)
    xor = V.zipWith xor

    complement = fmap complement

    shift v i = V.generate l (\j -> if (0 <= (j+i)) && ((j+i) < l)
                                        then v ! (j+i)
                                        else zeroBits
                              )
                    where l = V.length v


    rotate v i = V.generate l (\j -> v ! (j+i `mod` l))
                    where l = V.length v


    bitSizeMaybe = Just . V.length
    isSigned = const False

    -- this makes a vector only long enough to set the bit...
    bit i = V.generate (i+1) (\j -> j==i)
    testBit = (!)

    popCount = V.foldr (\b s -> if b then s+1 else s) 0


{-
convertChecker :: Word8 -> Bool
convertChecker w = (toBits . fromBits 8) w == w

shiftChecker :: Word8 -> Bool
shiftChecker w = (toBits . flip shift 3 . fromBits 8) w == flip shift 3 w

rotateChecker :: Word8 -> Bool
rotateChecker w = (toBits . flip rotate 10 . fromBits 8) w == flip rotate 10 w
-}
