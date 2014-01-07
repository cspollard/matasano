module Frequency where

import qualified Data.Map as M
import qualified Data.Char as C

counts :: Ord a => [a] -> M.Map a Int
counts = foldl (\m k -> M.insertWith (+) k 1 m) M.empty

freqs :: (Ord a) => M.Map a Int -> M.Map a Double
freqs m = M.map (\i -> fromIntegral i/tot) m
    where tot = fromIntegral . sum $ M.elems m

charCounts :: String -> M.Map Char Int
charCounts = foldl (\m k -> M.insertWith (+) k 1 m) charCountMap . map C.toLower . filter C.isAlpha

charFreqs :: String -> M.Map Char Double
charFreqs = freqs . charCounts

englCharFreq :: Char -> Double
englCharFreq c = M.findWithDefault 0.0 c englCharFreqMap

-- the first list of Doubles should be the expected values.
chi2 :: [Double] -> [Double] -> Double
chi2 xs ys = sum $ zipWith (\x y -> ((x-y)**2)/x) xs ys

englCharChi2 :: String -> Double
englCharChi2 s = chi2 (M.elems englCharFreqMap) (M.elems $ charFreqs s)

englCharFreqMap :: M.Map Char Double
englCharFreqMap = M.fromList englCharFreqList

englCharFreqList :: [(Char, Double)]
englCharFreqList =
  [('a', 0.08167),
    ('b', 0.01492),
    ('c', 0.02782),
    ('d', 0.04253),
    ('e', 0.12702),
    ('f', 0.02228),
    ('g', 0.02015),
    ('h', 0.06094),
    ('i', 0.06966),
    ('j', 0.00153),
    ('k', 0.00772),
    ('l', 0.04025),
    ('m', 0.02406),
    ('n', 0.06749),
    ('o', 0.07507),
    ('p', 0.01929),
    ('q', 0.00095),
    ('r', 0.05987),
    ('s', 0.06327),
    ('t', 0.09056),
    ('u', 0.02758),
    ('v', 0.00978),
    ('w', 0.02360),
    ('x', 0.00150),
    ('y', 0.01974),
    ('z', 0.00074)]

charCountMap :: M.Map Char Int
charCountMap = M.fromList
  [('a', 0),
    ('b', 0),
    ('c', 0),
    ('d', 0),
    ('e', 0),
    ('f', 0),
    ('g', 0),
    ('h', 0),
    ('i', 0),
    ('j', 0),
    ('k', 0),
    ('l', 0),
    ('m', 0),
    ('n', 0),
    ('o', 0),
    ('p', 0),
    ('q', 0),
    ('r', 0),
    ('s', 0),
    ('t', 0),
    ('u', 0),
    ('v', 0),
    ('w', 0),
    ('x', 0),
    ('y', 0),
    ('z', 0)]

