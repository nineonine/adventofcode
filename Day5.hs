module Day5 where

import Data.List
import Control.Monad

-- PART ONE --

atLeastThreeVowels :: String -> Bool
atLeastThreeVowels s = numOfVowels  >= 3 where
                         numOfVowels =  length $ filter isVowel s where
                             isVowel =  flip elem "aeiou"

hasDoubleLetter :: String -> Bool
hasDoubleLetter (x:y:rest)
        | x == y    = True
        | otherwise = hasDoubleLetter (y:rest)
hasDoubleLetter _   = False

noForbiddenStrings :: String -> Bool
noForbiddenStrings (x:y:rest)
        | [x,y] `elem` ["ab", "cd", "pq", "xy"] = False
        | otherwise = noForbiddenStrings (y:rest)
noForbiddenStrings _ = True

-- PART TWO --

atLeastTwoPairs :: String -> Bool
atLeastTwoPairs (x:y:rest) 
        | elem [x,y] $ zipWith (\a b -> [a,b]) rest (tail rest) = True
        | otherwise = atLeastTwoPairs (y:rest)
atLeastTwoPairs _ = False

oneBetweenPair :: String -> Bool
oneBetweenPair (a:b:c:rest) 
        | a == c    = True
        | otherwise = oneBetweenPair (b:c:rest)
oneBetweenPair _ = False        

countNiceStrings :: FilePath -> IO Int
countNiceStrings fp = do
    strings <- liftM lines $ readFile fp
    return . length $ filter nice strings where
        nice c = and $ map ( $ c ) criteriaList where
            -- criteriaList = [atLeastThreeVowels, hasDoubleLetter, noForbiddenStrings]
            criteriaList = [atLeastTwoPairs, oneBetweenPair]    -- PART TWO