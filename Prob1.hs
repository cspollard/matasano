import Data.ByteString.Base16.Lazy as B16
import Data.ByteString.Base64.Lazy as B64
import Data.ByteString.Lazy as BSL

main :: IO ()
main = print . B64.encode . fst . B16.decode =<< BSL.getContents
