module Day17 where

import Data.List
import Data.Function

main :: IO ()
main = do
    i <- fmap read . lines <$> readFile "day17input.txt" :: IO [Int]
    let part1  = sortBy (compare `on` length) . filter ( \l -> sum l == 150 ) $ subsequences i
        minLen = length $ head part1
        part2  = takeWhile (\l -> length l == minLen) part1
    mapM_ (print . length) [part1, part2]


