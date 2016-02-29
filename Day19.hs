module Day19 where

import Data.List.Utils
import Data.List
import Data.Monoid

type Replacement = (String, String)
type ReplacementsMap = [Replacement] -- 
type Molecule = String

getInput :: IO [String]
getInput = lines <$> readFile "day19input.txt"

parseReplMap :: [String] -> ReplacementsMap
parseReplMap [] = []
parseReplMap ss = flip map ss $ (,) 
                                <$> takeWhile (/=' ')
                                <*> last . words

parse :: [String] -> (ReplacementsMap, Molecule)
parse = (,) 
        <$> parseReplMap . takeWhile (/="")
        <*> last  


properPrepend :: [(Int, String)] -> Replacement -> (Int, String) -> Molecule
properPrepend xs (old,new) (i,_) = foldl' step [] xs
                                    where
                                    step acc (1, s) = acc <> s
                                    step acc (n, s) | n == i = acc <> new <> s | otherwise  = acc <> old <> s


allSubs :: Molecule -> ReplacementsMap -> [[Molecule]]
allSubs _   []              = []
allSubs mol ((old, new):xs) = newMols : allSubs mol xs
                             where
                             indexed = zip [1..] $ split old mol
                             newMols = tail $ map ( properPrepend indexed (old,new) ) indexed



main :: IO ()
main = do
       (rMap, mol) <- parse <$> getInput
       let part1 = length . nub . concat $ allSubs mol rMap
       print part1