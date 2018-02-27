module Day8 where


-- | number of characters in the code representation of the string literal
literalLength :: String -> Int
literalLength s = 2 + sum (prs $ show s)
    where
    prs ('\\':'\"'   :xs) = 1 : prs xs
    prs ('\\':'\\'   :xs) = 1 : prs xs
    prs ('\\':'x':_:_:xs) = 3 : prs xs
    prs (_           :xs) = prs xs
    prs []                = []

getStrings :: FilePath -> IO [String]
getStrings fp = lines <$> readFile fp


len :: String -> Int
-- len ('\\':xs) = 2 + len xs
len ('"':xs) = 2 + len xs
len ('\\':'x':_:_:xs) = 4 + len xs
len (_:xs) = 1 + len xs
len [] = 2
