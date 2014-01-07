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

bestEnglString :: [String] -> String
bestEnglString = argmin englCharChi2

bestEnglKey :: String -> (Char, Double)
bestEnglKey s = argminWithMin (englCharChi2 . boolsToStrASCII . xors (strToBools16 s) . charToBoolsASCII) "abcdefghijklmnopqrstuvwxyz"
