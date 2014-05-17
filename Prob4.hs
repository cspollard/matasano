import Crypto
import Data.List.Extras (argmin)
import Control.Applicative

main :: IO ()
main = do
    a <- getLine
    bs <- lines <$> readFile a

    -- print $ argmin snd (map (flip bestEnglKey alphabet) bs)
    print $ map (flip bestEnglKey alphabet) bs

    return ()
