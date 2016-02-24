{-# LANGUAGE BangPatterns #-}

module Main where

import Data.List


type Proportions = [Int]
data Ingredient = Ingredient {
                  capacity :: !Int
                , durability :: !Int
                , flavor :: !Int
                , texture :: !Int
                , calories :: !Int
                } deriving (Show)

getInput :: IO [Ingredient]
getInput = map parseIngr . lines <$> readFile "day15input.txt"

parseIngr :: String -> Ingredient
parseIngr = Ingredient
              <$> riw 2
              <*> riw 4
              <*> riw 6
              <*> riw 8
              <*> read . last . words
              where
              riw n = read . init . (!!n) . words


possibleCombos :: [Proportions]
possibleCombos = [ [a,b,c,d] | a <- [1..100], b <- [1..100], c <- [1..100], d <- [1..100], sum [a,b,c,d] == 100 ]

keepCaloriesLvl :: [Ingredient] -> Proportions -> Bool
keepCaloriesLvl is p = total == 500 
                       where
                       total = sum $ zipWith (*) p (map calories is)


totalScore :: [Ingredient] -> Proportions -> Int
totalScore !is !ps = product $ subNegs [totalCap, totalDur, totalFlav, totalText]
                   where 
                   getTotalIn f = sum $ zipWith (*) ps (map f is)
                   !totalCap     = getTotalIn capacity
                   !totalDur     = getTotalIn durability 
                   !totalFlav    = getTotalIn flavor
                   !totalText    = getTotalIn texture
                   subNegs      = map (\x -> if x < 1 then 0 else x)


main :: IO ()
main = do
    is <- getInput
    let part1 = maximum . foldl' (flip (:)) [] $ map (totalScore is) possibleCombos
        part2 = maximum . foldl' (flip (:)) [] . map (totalScore is) $ filter (keepCaloriesLvl is) possibleCombos
    print part1
    print part2







