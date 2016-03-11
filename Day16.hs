{-# LANGUAGE ViewPatterns #-}

module Day16 where

import Data.Char
import Data.Maybe
import Data.Monoid
import qualified Data.Map as M

loadInput :: IO [String]
loadInput = lines <$> readFile "day16input.txt"

parseAunt :: String -> M.Map String Int
parseAunt (words -> (_:auntNum:x:n1:y:n2:z:n3:_))  = M.fromList [("aunt", ri auntNum), (init x, rh n1), (init y, rh n2), (init z, rh n3)]
                                                     where
                                                     rh = digitToInt . head
                                                     ri = read . init

tickerTape :: M.Map String Int
tickerTape = M.fromList [("children",3),("cats",7),("samoyeds",2),("pomeranians",3),("akitas",0),("vizslas",0),("goldfish",5),("trees",3),("cars",2),("perfumes",1)]

findAunt :: [M.Map String Int] -> Maybe (M.Map String Int)
findAunt []     = Nothing
findAunt (x:xs) = if theOne x
                  then Just x
                  else findAunt xs

part2Checker :: String -> Int -> Bool
part2Checker s@"cats" = (<) $ tickerTape M.! s
part2Checker s@"trees" = (<) $ tickerTape M.! s
part2Checker s@"pomeranians" = (>) $ tickerTape M.! s
part2Checker s@"goldfish" = (>) $ tickerTape M.! s
part2Checker s = (==) $ tickerTape M.! s

theOne :: M.Map String Int -> Bool
-- theOne m = getAll $ M.foldMapWithKey (\ k v -> All $ tickerTape M.! k == v ) m' -- part 1
theOne m = getAll $ M.foldMapWithKey (\ k v -> All $ part2Checker k v ) m' -- part 2
           where m' = M.delete "aunt" m

main :: IO ()
main = do
    input <- loadInput
    let part1 = fromJust . findAunt $ map parseAunt input
    print part1
