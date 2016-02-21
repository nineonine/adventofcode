module Day9 where

import Data.List
import Data.Function

type City = String
type Route = (City,City,Int)

getInput :: IO [String]
getInput = lines <$> readFile "day9input.txt"

parseRoutes :: [String] -> [Route]
parseRoutes = map psr
                  where
                  psr = (,,)
                        <$> takeWhile ( /= ' ')
                        <*> ( !! 2) . words
                        <*> fmap (read :: String -> Int) last . words

setOfCities :: [Route] -> [City]
setOfCities cs = first:rest
                   where
                   first = ( \(a,_,_) -> a ) $ head cs
                   rest  = map ( \(_,b,_) -> b ) $ take 7 cs

mergeRoute :: [City] -> [(City, City)]
mergeRoute c1  = zip c1 $ tail c1

unmergeRoute :: [(City, City)] -> [City]
unmergeRoute cs = map fst cs ++ [lastEl] 
                    where  lastEl = snd $ last cs

lookupDistance :: [Route] -> (City,City) -> Int
lookupDistance rs (c1,c2) = case flip filter rs $ \(a,b,_) -> a == c1 && b == c2 of
    [] -> lookupDistance  rs (c2, c1)
    _  -> ( \(_,_,c) -> c )
               . head . filter ( \(_,b,_) -> b == c2 )
               $ filter ( \(a,_,_) -> a == c1 ) rs

fullDistance :: [Route] -> [(City,City)] -> Int
fullDistance rs full = sum $ map distance full
    where distance = lookupDistance rs

main :: IO ()
main = do
    routes <- parseRoutes <$> getInput
    let allPossible = map mergeRoute . permutations $ setOfCities routes
        distance = fullDistance routes
        result = maximumBy (compare `on` distance ) allPossible -- change to minimumBy to solve part 1 
    print $ unmergeRoute result    
    print $ distance result 
    return ()
