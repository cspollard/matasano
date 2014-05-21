module ToChar where

class ToChar tc where
    toChar :: tc -> Char

class FromChar tc where
    fromChar :: tc -> Char
