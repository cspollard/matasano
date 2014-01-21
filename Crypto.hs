module Crypto where
import Frequency
import Convert
import Data.List.Extras (argmin, argminWithMin)
import Control.Arrow

xor :: Bool -> Bool -> Bool
xor a b = (a && not b) || (b && not a)

xors :: [Bool] -> [Bool] -> [Bool]
xors bs key = zipWith xor bs (cycle key)

xorsRecurse :: [Bool] -> [[Bool]] -> [Bool]
xorsRecurse = foldl xors

bestEnglString :: [String] -> (String, Double)
bestEnglString [] = ("", 999999999)
bestEnglString ss = argminWithMin englCharChi2 . filter isValidASCII $ ss

bestEnglKey :: String -> [String] -> ((String, String), Double)
bestEnglKey s = argmin
        (englCharChi2
         <<< (boolsToStrASCII ||| boolsToStrASCII)
         <<< (id &&& xors (strToBools16 s))
         <<< charToBoolsASCII) "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"
