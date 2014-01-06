module Convert where

import Data.List.Split (chunksOf)
import qualified Data.Bits as B
import Data.Word (Word8)
import qualified Data.Map as M
import Data.Tuple (swap)

setBitIf :: Word8 -> (Bool, Int) -> Word8
setBitIf w (b, i) = if b then B.setBit w i else w

charToBools16 :: Char -> [Bool]
charToBools16 c = map (B.testBit (map16CW8 M.! c)) [3, 2, 1, 0]

charToBools64 :: Char -> [Bool]
charToBools64 c = map (B.testBit (map64CW8 M.! c)) [5, 4, 3, 2, 1, 0]

charToBoolsASCII :: Char -> [Bool]
charToBoolsASCII c = map (B.testBit (mapASCIICW8 M.! c)) [7, 6, 5, 4, 3, 2, 1, 0]

boolsToW8 :: [Bool] -> Word8
boolsToW8 bs = foldl setBitIf 0 $ zip (reverse bs) [0..7]

boolsToChar16 :: [Bool] -> Char
boolsToChar16 bs = M.findWithDefault '\0' (boolsToW8 bs) map16W8C

boolsToChar64 :: [Bool] -> Char
boolsToChar64 bs = M.findWithDefault '\0' (boolsToW8 bs) map64W8C

boolsToCharASCII :: [Bool] -> Char
-- boolsToCharASCII bs = M.findWithDefault '\0' (boolsToW8 bs) mapASCIIW8C
boolsToCharASCII = toEnum . fromIntegral . boolsToW8

strToBools16 :: String -> [Bool]
strToBools16 = concatMap charToBools16

strToBools64 :: String -> [Bool]
strToBools64 = concatMap charToBools64

boolsToStr16 :: [Bool] -> String
boolsToStr16 = map boolsToChar16 . chunksOf 4

boolsToStr64 :: [Bool] -> String
boolsToStr64 = map boolsToChar64 . chunksOf 6

boolsToStrASCII :: [Bool] -> String
boolsToStrASCII = map boolsToCharASCII . chunksOf 8

map16CW8 :: M.Map Char Word8
map16CW8 = M.fromList list16

map16W8C :: M.Map Word8 Char
map16W8C = M.fromList (map swap list16)

map64CW8 :: M.Map Char Word8
map64CW8 = M.fromList list64

map64W8C :: M.Map Word8 Char
map64W8C = M.fromList (map swap list64)

mapASCIICW8 :: M.Map Char Word8
mapASCIICW8 = M.fromList listASCII

mapASCIIW8C :: M.Map Word8 Char
mapASCIIW8C = M.fromList (map swap listASCII)


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

listASCII :: [(Char, Word8)]
listASCII =
   [(' ', 032),
    ('!', 033),
    ('"', 034),
    ('#', 035),
    ('$', 036),
    ('%', 037),
    ('&', 038),
    ('\'', 039),
    ('(', 040),
    (')', 041),
    ('*', 042),
    ('+', 043),
    (',', 044),
    ('-', 045),
    ('.', 046),
    ('/', 047),
    ('0', 048),
    ('1', 049),
    ('2', 050),
    ('3', 051),
    ('4', 052),
    ('5', 053),
    ('6', 054),
    ('7', 055),
    ('8', 056),
    ('9', 057),
    (':', 058),
    (';', 059),
    ('<', 060),
    ('=', 061),
    ('>', 062),
    ('?', 063),
    ('@', 064),
    ('A', 065),
    ('B', 066),
    ('C', 067),
    ('D', 068),
    ('E', 069),
    ('F', 070),
    ('G', 071),
    ('H', 072),
    ('I', 073),
    ('J', 074),
    ('K', 075),
    ('L', 076),
    ('M', 077),
    ('N', 078),
    ('O', 079),
    ('P', 080),
    ('Q', 081),
    ('R', 082),
    ('S', 083),
    ('T', 084),
    ('U', 085),
    ('V', 086),
    ('W', 087),
    ('X', 088),
    ('Y', 089),
    ('Z', 090),
    ('[', 091),
    ('\\', 092),
    (']', 093),
    ('^', 094),
    ('_', 095),
    ('`', 096),
    ('a', 097),
    ('b', 098),
    ('c', 099),
    ('d', 100),
    ('e', 101),
    ('f', 102),
    ('g', 103),
    ('h', 104),
    ('i', 105),
    ('j', 106),
    ('k', 107),
    ('l', 108),
    ('m', 109),
    ('n', 110),
    ('o', 111),
    ('p', 112),
    ('q', 113),
    ('r', 114),
    ('s', 115),
    ('t', 116),
    ('u', 117),
    ('v', 118),
    ('w', 119),
    ('x', 120),
    ('y', 121),
    ('z', 122),
    ('{', 123),
    ('|', 124),
    ('}', 125),
    ('~', 126),
    
    ('\0', 000),
    ('\a', 007),
    ('\b', 010),
    ('\t', 011),
    ('\n', 012),
    ('\v', 013),
    ('\f', 014),
    ('\r', 015)]

{-
    (001, '^A'),
    (002, '^B'),
    (003, '^C'),
    (004, '^D'),
    (005, '^E'),
    (006, '^F'),
    (016, '^N'),
    (017, '^O'),
    (020, '^P'),
    (021, '^Q'),
    (022, '^R'),
    (023, '^S'),
    (024, '^T'),
    (025, '^U'),
    (026, '^V'),
    (027, '^W'),
    (030, '^X'),
    (031, '^Y'),
    (032, '^Z'),
    (033, '^[')]
    (034, '^\'),
    (035, '^]'),
    (036, '^^'),
    (037, '^_'),
    (177, '^?'),
-}
