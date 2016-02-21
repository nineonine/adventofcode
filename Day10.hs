module Day10 where

import Data.Char

transform :: String -> String
transform     []     = []
transform    [x]     = '1':[x]
transform s@(x:y:xs) = if x /= y 
                          then '1':x: transform (y:xs) 
                          else intToDigit n : x : transform rest
                            where
                            n = length $ takeWhile (x==) s
                            rest = drop n s
 
times :: Int -> (String -> String) -> String -> [String]
times n f i = take n $ iterate f i

main :: IO ()
main = do
    let r = length . last $ (51 `times` transform) "1321131112" -- 41 times for part 1
    print r
