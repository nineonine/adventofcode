{-# LANGUAGE ViewPatterns #-}

module Main where

import Prelude hiding (null, length)
import Data.Char
import Data.Foldable (toList)
import Data.Sequence

type Password = Seq Char

incCharAt :: Int -> Password -> Password
incCharAt n s = let inc = (chr . (+1) . ord) in adjust inc n s

wrapAroundAt :: Int -> Password -> Password
wrapAroundAt n = update n 'a'

incPass :: Password -> Int -> Maybe Password
incPass (null -> True) _ = Nothing
incPass s n
    | n > (length s - 1) = Nothing
    | n < 0 = Nothing
    | s `index` n == 'z' = let s' = wrapAroundAt n s in incPass s' (n - 1)
    | otherwise = return $ incCharAt n s

-- password requirements --
straightOfThree :: Password -> Bool
straightOfThree (toList -> (x:y:z:rest)) = (ord x + 1 == ord y && ord y + 1 == ord z) || straightOfThree (fromList (y:z:rest))
straightOfThree _ = False

noIOL :: Password -> Bool
noIOL p = not $ any (`elem` p) "iol"

hasTwo :: Int -> Password -> Bool
hasTwo _ (null -> True) = False
hasTwo _ (length -> 1) = False
hasTwo 2 _ = True
hasTwo n (toList -> (x:y:rest))
    | n == 1 && x == y = True
    | otherwise = if x == y then hasTwo (n+1) (fromList rest) else hasTwo n (fromList (y:rest))

hasTwoPairs :: Password -> Bool
hasTwoPairs = hasTwo 0

satisfies :: Password -> Bool
satisfies p = all ($ p) [straightOfThree, noIOL, hasTwoPairs]
--------------------------

nextPassword :: Password -> Maybe Password
nextPassword p = let Just p' = incPass p ( length p - 1 )
                 in if satisfies p
                    then Just p
                    else nextPassword p'



main :: IO ()
main = do
    let input = fromList "hepxcrrq"
        Just p1 = nextPassword input
        Just next = incPass p1 (length p1 - 1)
    print p1
    print $ nextPassword next
