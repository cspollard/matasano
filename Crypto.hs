module Crypto where
import Frequency
import Convert
import Data.List.Extras (argmin, argminWithMin)
import Data.List.Split (chunksOf)
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
bestEnglKey str keys = argminWithMin b $
            -- MISTAKE
            -- this needs to be the xor'd string.
            zip (repeat str) keys
        where b (s, k) = englCharChi2 $ boolsToStrASCII $ xors (strToBoolsASCII s) (strToBoolsASCII k)

alphabet = map (:[]) "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"
