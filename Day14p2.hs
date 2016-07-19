module Day14p2 where

import Prelude hiding (replicate)
import Data.DList hiding (head, map)
import Data.Foldable (foldl')
import Data.Function (on, fix)
import qualified Data.Map.Strict as Map
import qualified Data.List as L

r1 = Reindeer "rudolf" 10 5 3

type ContestChart = Map.Map String Score
data Action = Fly | Rest deriving (Show)
type MovingPattern = DList Speed
type Distance = Int
type Speed = Int
type Time  = Int
type Score = Int
data Reindeer = Reindeer
                { name  :: String
                , speed :: Speed
                , fly   :: Time
                , rest  :: Time
              } deriving (Show, Eq)

initialChart :: [Reindeer] -> ContestChart
initialChart []  = Map.empty
initialChart rs  = foldl' ( \ chart rnd@(Reindeer n s f r) -> Map.insert n 0 chart ) Map.empty rs

parseToReindeer :: String -> Reindeer
parseToReindeer = Reindeer
                    <$> head . words
                    <*> read . flip (!!) 3 . words
                    <*> read . flip (!!) 6 . words
                    <*> read . flip (!!) 1 . reverse . words

-- stream of reindeer running pattern
stream :: Reindeer -> MovingPattern
stream rnd = replicate (fly rnd) (speed rnd) `append` replicate (rest rnd) 0 `append` stream rnd

coveredDistance :: Time -> MovingPattern -> Distance
coveredDistance 0 _ = 0
coveredDistance t p = sum . take t $ toList p

getDistances :: [Reindeer] -> Time -> [(String, Distance)]
getDistances _ 0  = []
getDistances rs n = [(name r, coveredDistance n $ stream r ) | r <- rs]

getLeaders :: [(String, Distance)] -> [String]
getLeaders [] = []
getLeaders rs = let maxVal = snd $ L.maximumBy (compare `on` snd) rs
                in map fst $ filter ( \ v -> snd v == maxVal ) rs

updateChart :: ContestChart -> [String] -> ContestChart
updateChart c [] = c
updateChart c ns = foldl' (flip (Map.adjust (1 +) )) c ns

runContest :: [Reindeer] -> ContestChart -> Time -> ContestChart
runContest rs c t = foldl' ( tick rs ) c (take t [1..])
                   where
                   tick rs chrt t = updateChart chrt (getLeaders $ getDistances rs t)


main :: IO ()
main = do
    reindeers <- map parseToReindeer . lines <$> readFile "day14input.txt"
    let final = L.maximumBy (compare `on` snd) . Map.toList $ runContest reindeers (initialChart reindeers) 2503
    print final
    return ()
