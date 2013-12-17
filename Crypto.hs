module Crypto where

import qualified Data.Bits as B
import Data.Word (Word8)
import qualified Data.Map as M
import Data.Tuple (swap)

strToW8s16 :: String -> [Word8]
strToW8s16 = reverse . strToW8s16' . reverse

-- THIS IS SUPPOSED TO BE BACKWARDS!!!
strToW8s16' :: String -> [Word8]
strToW8s16' "" = []
-- don't bitshift if this is the last char.
strToW8s16' [c] = [map16CW8 M.! c]
strToW8s16' (c:c':cs) = w : strToW8s16' cs
    -- Note that this is in the reverse direction!
    where w = (map16CW8 M.! c) B..|. B.shiftL (map16CW8 M.! c') 4

strToW8s64 :: String -> [Word8]
strToW8s64 = map (\c -> map64CW8 M.! c)

w8sToStr16 :: [Word8] -> String
w8sToStr16 [] = ""
w8sToStr16 (w:ws) = c : c' : w8sToStr16 ws
    where
        c = map16W8C M.! B.shiftR w 4
        c' = map16W8C M.! (w B..&. 15)

w8sToStr64 :: [Word8] -> String
w8sToStr64 = map (\c -> map64W8C M.! c)

-- when we're starting at a new word.
w8sToStr64' :: Int -> [Word8] -> String
w8sToStr64' i (w:w':ws) = case i of
                            0 -> c : w8sToStr64 
    where
        c = map64W8C $ B.shift
        w'' = 'a'



map16CW8 :: M.Map Char Word8
map16CW8 = M.fromList list16

map16W8C :: M.Map Word8 Char
map16W8C = M.fromList (map swap list16)

map64CW8 :: M.Map Char Word8
map64CW8 = M.fromList list64

map64W8C :: M.Map Word8 Char
map64W8C = M.fromList (map swap list64)

list16 :: [(Char, Word8)]
list16 = 
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


list64 :: [(Char, Word8)]
list64 = 
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
