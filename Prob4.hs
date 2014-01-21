import Crypto
import Data.List.Extras (argminWithMin)
import Control.Applicative

main :: IO ()
main = do
    a <- getLine
    bs <- lines <$> readFile a

    print $ argminWithMin fst (map bestEnglKey bs)

    return ()
