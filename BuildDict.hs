import Data.Crypto.English
import System.Environment (getArgs)
import qualified Data.Map as M
import Data.List (foldl')
import Control.Monad (liftM)
import Control.Arrow ((&&&))
import Data.Serialize (encode)
import qualified Data.ByteString as BS (writeFile)
import Data.Char (toLower)


validASCII :: Char -> Bool
validASCII c = c >= ' ' && c <= '~'

getFreqDict :: String -> M.Map Char Int
getFreqDict = foldl' (\m k -> M.insertWith (+) k 1 m) M.empty . map toLower . filter validASCII

getProbDict :: String -> M.Map Char Double
getProbDict s = M.map (\i -> fromIntegral i/tot) m
    where
        m = getFreqDict s
        tot = fromIntegral . sum $ M.elems m

main :: IO ()
main = do
    files <- getArgs

    let (finnames, foutname) = (tail &&& head) . reverse $ files

    indata <- liftM concat $ mapM readFile finnames

    let d = getProbDict indata

    print d

    let outdata = encode d

    BS.writeFile foutname outdata
