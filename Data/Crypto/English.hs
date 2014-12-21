module Data.Crypto.English where

import qualified Data.Map as M
import qualified Data.Char as C
import Data.Crypto.Frequency

charChi2 :: M.Map Char Double -> M.Map Char Double -> Double
charChi2 expected observed = if expected `M.difference` observed /= M.empty
                                then M.foldr (+) 0.0 $ M.intersectionWith chi2 expected observed
                                else 1e100


engChi2 :: String -> Double
engChi2 = charChi2 engCharFreqMap . charFreqs . map C.toLower


validEnglish :: String -> Bool
validEnglish = all (flip M.member engCharFreqMap . C.toLower)


counts :: String -> M.Map Char Int
counts = foldl (\m k -> M.insertWith (+) k 1 m) M.empty

freqs :: (Ord a) => M.Map a Int -> M.Map a Double
freqs m = M.map (\i -> fromIntegral i/tot) m
    where tot = fromIntegral . sum $ M.elems m

charCounts :: String -> M.Map Char Int
charCounts = foldl (\m k -> M.insertWith (+) k 1 m) M.empty

charFreqs :: String -> M.Map Char Double
charFreqs = freqs . charCounts

-- the first list of Doubles should be the expected values.
-- return a very large number if x <= 0
chi2 :: Double -> Double -> Double
chi2 expected observed = (observed - expected)**2/expected
