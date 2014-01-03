module Crypto where

xor :: Bool -> Bool -> Bool
xor a b = (a && not b) || (b && not a)
