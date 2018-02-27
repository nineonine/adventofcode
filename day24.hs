module Main where

import Control.Monad
import Data.Function
import Data.List

getInput :: IO [Int]
getInput = fmap read . lines <$> readFile "day24input.txt"

sortedPossibleGroups :: [Int] -> Int -> [[Int]]
sortedPossibleGroups [] _ = []
sortedPossibleGroups xs n = sortBy (compare `on` length) . filter (\x -> sum x == n) $ subsequences xs

lesserPackages :: Int -> [Int] -> [[Int]]
lesserPackages _ [] = []
lesserPackages n xs = guard (length xs == n) >> return xs

quantumEnt :: [Int] -> Int
quantumEnt = product

main :: IO ()
main = do
    input <- getInput
    let groups = sortedPossibleGroups input (sum input `div` 4 ) -- change to 3 for part 1
        minLen = length $ head groups
        result = minimumBy (compare `on` quantumEnt) $ lesserPackages minLen =<< groups
    print $ quantumEnt result
