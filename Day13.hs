{-# LANGUAGE OverloadedStrings #-}

module Day13 where

import Data.List as L
import Data.Map as M
import Data.ByteString as BS
import Data.ByteString.Char8 as CBS

getInput :: IO [ByteString]
getInput = CBS.lines <$> BS.readFile "day13input.txt"

type Name = ByteString
type HappynessLevelMap = M.Map Name Int
type RelationsMap = M.Map Name HappynessLevelMap
type RelationRecord = (Name, Name, Int)

myRelMap :: [Name] -> HappynessLevelMap
myRelMap  = L.foldl' go M.empty 
              where
              go m n = M.insert n 0 m

stringToRec :: BS.ByteString -> RelationRecord
stringToRec = (,,)
              <$> L.head . CBS.words
              <*> BS.init . L.last . CBS.words
              <*> ( \l -> loseOrGain (l !! 2) (l !! 3) ) . CBS.words
                  where 
                    loseOrGain verb value = case verb of
                        "gain" -> read $ CBS.unpack value :: Int
                        _     -> negate $ read $ CBS.unpack value :: Int

step :: RelationsMap -> RelationRecord -> RelationsMap
step r (n1,n2,i) =  if M.member n1 r
                       then M.insert n1 (M.insert n2 i $ r ! n1) r
                       else M.insert n1 (M.singleton n2 i) r

bindPeople :: [Name] -> [(Name, Name)]
bindPeople []       = []
bindPeople a@(n:ns) = (n, L.last ns):L.zip a (L.tail a)

totalHappiness :: RelationsMap -> [(Name, Name)] -> Int
totalHappiness rm = L.sum . L.map (getHappyUnits rm)

getHappyUnits :: RelationsMap -> (Name,Name) -> Int
getHappyUnits m (n1, n2) = ((m ! n1) ! n2) + ((m ! n2) ! n1)

buildRelationsMap :: [RelationRecord] -> RelationsMap
buildRelationsMap = L.foldl' step M.empty

main :: IO ()
main = do
    -- relMap <- (buildRelationsMap . L.map stringToRec) <$> getInput -- part 1
    relMap <- ( (\m -> M.insert "Me" (myRelMap (L.nub $ M.keys m)) m) . M.map (M.insert "Me" 0) . buildRelationsMap . L.map stringToRec) <$> getInput -- part 2
    let sittingCombos = L.permutations . L.nub $ M.keys relMap -- :: [[Name]]
        result = L.maximum $ L.map (totalHappiness relMap . bindPeople) sittingCombos
    print result