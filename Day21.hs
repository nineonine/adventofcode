
module Main where

import Data.List.Split (splitOn)
import Control.Monad
import D21types

toItem :: [String] -> Item
toItem xs = Item c d a
            where
            rd = read :: String -> Int
            c = rd $ xs !! 1
            d = rd $ xs !! 2
            a = rd $ last xs

parseToArmory :: [[String]] -> Armory
parseToArmory = (,,)
                <$> map (toItem . words) . head
                <*> (:) mempty . map (toItem . words) . flip (!!) 1 -- adding empty item because you can be without armor
                <*> (:) mempty . map (toItem . words) . last        -- same here - can be 0-2 rings

getInput :: IO [[String]]
getInput = map tail . splitOn [""] . lines <$> readFile "day21input.txt"

part1 :: Armory -> Int
part1 armry = minimum .  map ( cost . mconcat ) . filter (\set -> (player `equipedIn` set) `defeats` boss) $ allSets armry

part2 :: Armory -> Int
part2 armry = maximum .  map ( cost . mconcat ) . filter (not . (\set -> (player `equipedIn` set) `defeats` boss)) $ allSets armry

main :: IO ()
main = do
    armory <- parseToArmory <$> getInput
    print $ part1 armory
    print $ part2 armory
