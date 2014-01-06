import Convert

main :: IO ()
main = do
    a <- getLine

    putStrLn $ (boolsToStr64 . strToBools16) a
