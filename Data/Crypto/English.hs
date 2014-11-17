module English where

import qualified Data.Map as M
import qualified Data.Char as C
import qualified Data.ByteString as BS
import Data.Serialize (decode)

charChi2 :: M.Map Char Double -> String -> Double
charChi2 m s = chi2 (M.elems m) (M.elems $ charFreqs s)

valid :: String -> Bool
valid = all (\c -> c >= ' ' && c <= '~')

counts :: Ord a => [a] -> M.Map a Int
counts = foldl (\m k -> M.insertWith (+) k 1 m) M.empty

freqs :: (Ord a) => M.Map a Int -> M.Map a Double
freqs m = M.map (\i -> fromIntegral i/tot) m
    where tot = fromIntegral . sum $ M.elems m

charCounts :: String -> M.Map Char Int
charCounts = foldl (\m k -> M.insertWith (+) k 1 m) M.empty . map C.toLower . filter C.isAlpha

charFreqs :: String -> M.Map Char Double
charFreqs = freqs . charCounts

charFreq :: M.Map Char Double -> Char -> Double
charFreq = flip (M.findWithDefault 0.0)

-- the first list of Doubles should be the expected values.
chi2 :: [Double] -> [Double] -> Double
chi2 xs ys = sum $ zipWith (\x y -> ((x-y)**2)/x) xs ys

loadDict :: String -> IO (Either String (M.Map Char Double))
loadDict s = return . decode =<< BS.readFile s
