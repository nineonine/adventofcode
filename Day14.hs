{-# LANGUAGE FlexibleContexts #-}

module Day14 where

import Data.Maybe
import Data.List
import Data.Function
import Data.Map.Lazy as M hiding (map, filter, foldl')

type Name = String
data Action = Move | Rest deriving Show
type RecordBoard = M.Map Name Int -- Reindeer Name => Points
type Reindeer = (Name, Int, Int, Int) -- (Name, Speed, MoveTime, RestTime)
type ReindeerMap = M.Map Name Reindeer
type Distance = Int
type Time = Int

rname :: Reindeer -> Name
rname (a, _, _, _) = a

rspeed :: Reindeer -> Int
rspeed (_, s, _, _) = s

getInput :: IO [String]
getInput = lines <$> readFile "day14input.txt"

emptyBoard :: [Reindeer] -> RecordBoard
emptyBoard rs = M.fromList [(name, 0) | (name, _, _, _) <- rs] 

parseToReindeer :: String -> Reindeer
parseToReindeer = (,,,) 
                      <$> head . words
                      <*> read . flip (!!) 3 . words
                      <*> read . flip (!!) 6 . words
                      <*> read . flip (!!) 1 . reverse . words

countDistance :: Time -> Distance -> Action -> Reindeer -> Distance
countDistance 0 d _ _                       = d 
countDistance t d Move r@(_, s, mt, _) = if t < mt 
                                          then d + abs (s * (t - mt))
                                          else s * mt + countDistance (t-mt) d Rest r
countDistance t d Rest r@(_, _, _, rt) = if t < rt 
                                          then d
                                          else countDistance (t-rt) d Move r 


roadScheme :: Action -> Time -> Int -> Int -> [Maybe Int]
roadScheme _ 0 _ _ = []
roadScheme Move n mt rt 
    | n > mt  = Just mt : roadScheme Rest (n - mt) mt rt
    | n < mt  = [Just (mt - n)]
    | otherwise = [Just mt]
roadScheme Rest n mt rt 
    | n > rt  = Nothing : roadScheme Move (n - rt) mt rt
    | n < rt  = [Nothing]
    | otherwise = [Nothing]   


-- HAS TO BE REIMPLEMENTED

distance :: Int -> Reindeer -> Distance
distance t (_, s, mt, rt) = (*s) . sum . catMaybes $ roadScheme Move t mt rt

-- calculate covered distance for all rs :: [Reindeer] -> Time -> [(Name, Distance)] 
-- HAVE TO BE REIMPLEMENTED
coveredDist :: Time -> [Reindeer] -> [(Name, Distance)]
coveredDist t = map namesAndDistances 
                where 
                namesAndDistances = (,) <$> rname <*> distance t

-- >> filter out all winners (with same distances) :: [(Name, Distance)] -> [Name]
getLeading :: [(Name, Distance)] -> [Name]
getLeading xs = map fst $ filter ( \a -> snd a == snd biggest) xs
                where 
                biggest = maximumBy (compare `on` snd) xs

-- >> upgate board records for all leading :: [Name] -> RecordBoard -> RecordBoard
updateBoard :: RecordBoard -> [Name] -> RecordBoard
updateBoard = foldl' step 
                where 
                step b n = M.adjust (+1) n b

runPart2 :: [Reindeer] -> RecordBoard -> [Time] -> RecordBoard
runPart2 rs = foldl' updateScores
                where
                updateScores b t = updateBoard b . getLeading $ coveredDist t rs
                       
main :: IO ()
main = do
    reindeers <- map parseToReindeer <$> getInput
    let getDistance = countDistance 2503 0 Move
        board       = emptyBoard reindeers
        part1       = maximum $ map getDistance reindeers
        part2       = snd . maximumBy (compare `on` snd) . M.toList $ runPart2 reindeers board [0..2503]
    print part1
    print $ M.toList $ runPart2 reindeers board [0..2503]
    print part2
