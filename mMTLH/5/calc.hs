main = do
    principalStr <- getLine
    -- readが返す型は `Read` のインスタンスである。型注釈によってその型を指定する。
    -- read :: Read a => String -> a
    let principal = read principalStr :: Double
    interestStr <- getLine
    let interest = read interestStr :: Double
    yearsStr <- getLine
    let years = read yearsStr :: Integer
    -- print :: Show a => a -> IO ()
    print (principal * (1 + interest / 100) ^ years)
