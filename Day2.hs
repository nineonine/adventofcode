{-# LANGUAGE OverloadedStrings #-}

module Day2 where

import Data.Text as T hiding (map, lines, foldl1')
import Data.List as L
import Control.Monad (liftM)

-- PART 1

-- parse the line and turn it into 3 dimensions
parseDimensions :: String -> [Int] 
parseDimensions = sort . map readInt . splitOn "x" . pack
    where readInt = (read :: String -> Int) . unpack

-- calculate necessary paper for wrapping - surface area : 2*l*w + 2*w*h + 2*h*l
getSurfaceArea :: [Int] -> Int
getSurfaceArea [a, b, c] = 2*a*b + 2*b*c + 2*a*c

-- calculate extra paper for wrapping
getSlack :: [Int] -> Int
getSlack [a, b, c] = a*b

-- surface + slack
getWithSlack :: [Int] -> Int
getWithSlack = \x -> getSurfaceArea x + getSlack x

runComputationForPaper :: FilePath -> IO Int
runComputationForPaper fp = do
	dims <- liftM lines $ readFile fp
	return . foldl1' (+) $ map (getWithSlack . parseDimensions) dims


-- PART 2

-- count amt of ribbon for present
ribbonForPresent :: [Int] -> Int
ribbonForPresent [a, b, c] = 2*a + 2*b

-- count amt of ribbon for bow
ribbonForBow :: [Int] -> Int
ribbonForBow [a, b, c] = a*b*c

-- combinator for getting the necessary amt of ribbon for 1 present
ribbonForOne :: [Int] -> Int
ribbonForOne = \x -> ribbonForPresent x + ribbonForBow x

runComputationForRibbon :: FilePath -> IO Int
runComputationForRibbon fp = do
	dims <- liftM lines $ readFile fp
	return . foldl1' (+) $ map (ribbonForOne . parseDimensions) dims





