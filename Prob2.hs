{-# LANGUAGE OverloadedStrings #-}

import qualified Data.ByteString.Base16.Lazy as B16
import qualified Data.ByteString.Lazy.Char8 as CH8
import Data.Crypto.ByteString
import System.Environment (getArgs)

main :: IO ()
main = do
    bs:key:_ <- map (fst . B16.decode . CH8.pack) `fmap` getArgs

    print . B16.encode $ xorBS bs key
