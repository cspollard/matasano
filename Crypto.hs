module Crypto where

xor :: Bool -> Bool -> Bool
xor a b = (a && not b) || (b && not a)

xors :: [Bool] -> [Bool] -> [Bool]
xors bs key = zipWith xor bs (cycle key)

xorsRecurse :: [Bool] -> [[Bool]] -> [Bool]
xorsRecurse = foldl xors
