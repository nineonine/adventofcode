{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}

module Main where

import Data.List
import Data.Either
import Text.Parsec
import Text.Parsec.String
import Control.Monad
import System.Environment
import GHC.Generics (Generic)
import Control.Parallel.Strategies hiding (NFData(..))
import Control.DeepSeq


-- DATA

data Action = Toggle | TurnOn | TurnOff deriving (Show, Generic, NFData)
data Status = On | Off deriving (Show, Eq, Generic, NFData)
data Coords = Coords Int Int deriving (Show, Eq, Generic, NFData)

data Light = Light { 
          coords :: Coords
        , status :: Status 
        } deriving (Show, Generic, NFData)

instance Eq Light where
    Light c1 s1 == Light c2 s2 = c1 == c2

type Instruction = (Coords, Coords, Action) 
type LightBoard = [Light]


-- HELPER FUNCTIONS

parseAction :: String -> Action
parseAction "toggle"   = Toggle
parseAction "turn off" = TurnOff
parseAction "turn on"  = TurnOn

makeBoard :: Int -> Int -> [Light]
makeBoard w h = [ Light (Coords x y) Off | x <- [0..w-1], y <- [0..h-1] ] 

getArea :: Instruction -> [Coords]
getArea (Coords x1 y1, Coords x2 y2, action) = [Coords x y | x <- [x1..x2], y <- [y1..y2] ]

isIn :: Light -> [Coords] -> Bool
isIn _ []                = False
isIn (Light coords _) cs = coords `elem` cs

actionFrom :: Instruction -> Action
actionFrom ( _ , _ , a ) = a

withApplied :: Light -> Action -> Light
withApplied (Light c On) TurnOn   = Light c On
withApplied (Light c On)   _      = Light c Off
withApplied (Light c Off) TurnOff = Light c Off
withApplied (Light c Off)   _     = Light c On

-- PARSER


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
    return (Coords x1 y1, Coords x2 y2, parseAction action)

updateBoard :: LightBoard -> Instruction -> LightBoard
updateBoard lboard i = map update lboard `using` parList rseq
    where update l = if l `isIn` appliedArea
                     then l `withApplied` ( actionFrom i )
                     else l
                     where appliedArea = getArea i


-- updateBoard :: LightBoard -> Instruction -> LightBoard
-- updateBoard lboard i = do 
--     let appliedArea = getArea i
--     light <- lboard
--     if light `isIn` appliedArea
--         then return $ light `withApplied` ( actionFrom i )
--         else return light


setupLights :: LightBoard -> [Instruction] -> LightBoard
setupLights lboard []     = lboard
setupLights lbrd instrs   = foldl' updateBoard lbrd instrs

-- IO


runSetup :: FilePath -> IO Int
runSetup fp = do
    Right instructions <- parseFromFile ( instructionParser `sepBy` newline ) fp
    return . length . filter ( \l -> status l == On ) $ setupLights (makeBoard 1000 1000) instructions

main :: IO ()
main = do
    [f] <- getArgs
    result <- runSetup f
    putStrLn $ show result

