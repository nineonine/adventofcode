module Main where

stream :: Int -> [Int]
stream 0 = []
stream n = n : stream next
           where
           next = n * 252533 `mod` 33554393

diagonals :: [[Int]]
diagonals = gen [1]
            where
            gen n = n : gen (take (length n + 1) [(head n + length n)..])

safeIndex :: Int -> [Int] -> Int
safeIndex n xs = if length xs <= n
                    then 0
                    else xs !! n

getColumn :: Int -> [Int]
getColumn 0 = []
getColumn n = dropWhile (==0) $ map (safeIndex $ n-1) diagonals

         -- Row -> Col -> Index
getIndex :: Int -> Int -> Int
getIndex n m = getColumn m !! (n-1)

findCode :: Int -> Int -> Int
findCode n m = stream 20151125 !! i
            where
            i = getIndex n m - 1


main :: IO ()
main = do
    let part1 = findCode 2978 3083
    print part1
