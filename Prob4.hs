import Crypto
import Data.List.Extras (argminWithMin)
import Control.Monad (liftM)

main :: IO ()
main = do
    a <- getLine
    bs <- liftM lines $ readFile a

    print $ argminWithMin snd (map bestEnglKey bs)

    return ()
