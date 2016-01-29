{-# LANGUAGE GADTs #-}

module Main where

import Control.Monad
import Text.Parsec
import Text.Parsec.String
import System.Environment
import qualified Data.Vector.Unboxed as V
import qualified Data.Vector.Unboxed.Mutable as M


type Screen = M.IOVector Int
type Action = Int -> Int
type Coords = (Int, Int)
type Area = (Coords, Coords) 
type Instruction = (Area, String)

-- Helper functions

getInstructions :: IO [Instruction]
getInstructions = do
        Right instructions <- parseFromFile ( instructionParser `sepBy` newline ) "day6input.txt"
        return instructions

getAction :: String -> Action
getAction "toggle"   = (+2)
getAction "turn on"  = (+1)
getAction "turn off" = \n -> case n of 
    0 -> 0
    _ -> subtract 1 n


-- Instruction Parser
instructionParser :: Parsec String () Instruction
instructionParser = do
    action <- (try $ string "toggle") <|> (try $ string "turn off") <|> (try $ string "turn on")
    space
    x1 <- liftM read $ many alphaNum
    char ','
    y1 <- liftM read $ many alphaNum
    string " through "
    x2 <- liftM read $ many alphaNum
    char ','
    y2 <- liftM read $ many alphaNum
    return $ ( ( (x1, y1), (x2, y2) ) , action) 

-- generate vector for storing lights
initScreen :: IO Screen
initScreen = M.replicate 1000000 0


processScreen :: Screen -> [Instruction] -> IO ()
processScreen s = mapM_ process 
    where process ( ((x1, y1), (x2, y2)) , a ) = sequence_ [ M.modify s (getAction a) (x+y*1000) | x <- [x1..x2], y <- [y1..y2] ]


run :: FilePath -> IO Int
run fp = do
        Right instructions <- parseFromFile ( instructionParser `sepBy` newline ) fp
        s <- initScreen
        new <- processScreen s instructions >> V.freeze s
        return $ V.sum new -- :: IO Int


main :: IO ()
main = do
    [f] <- getArgs
    result <- run f
    putStrLn $ show result

