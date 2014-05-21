import Base64 as B64
import Base16 as B16

main :: IO ()
main = do
    let conv16To64 = map B64.toChar . B64.unpack . B16.pack . map B16.fromChar

    print . conv16To64 =<< getLine
