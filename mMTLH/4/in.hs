main = do
    c <- getContents
    putStr (unlines (reverse (lines c)))
