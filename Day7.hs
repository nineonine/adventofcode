{-# LANGUAGE GADTs              #-}
{-# LANGUAGE StandaloneDeriving #-}

module Main where

import           Data.Bits
import           Data.Char
import           Data.List          hiding (delete)
import           Data.Map           hiding (map)
import           System.Environment
import           Text.Parsec        hiding (Empty)
import           Text.Parsec.String

loadInput :: IO String
loadInput = readFile "day7input.txt"

type ID = String
type Parsed = Parsec String ()


data Source where
 Wire   :: ID -> Source -> Source
 Raw    :: Int -> Source
 Empty  :: Source
 AND    :: Source -> Source -> Source
 OR     :: Source -> Source -> Source
 NOT    :: Source -> Source
 LSHIFT :: Source -> Int -> Source
 RSHIFT :: Source -> Int -> Source
deriving instance Show Source

reduceSource :: Source -> Int
reduceSource (Wire _ src) = reduceSource src
reduceSource (Raw  int   ) = int
reduceSource Empty         = 0
reduceSource (s1 `AND` s2) = reduceSource s1 .&. reduceSource s2
reduceSource (s1 `OR`  s2) = reduceSource s1 .|. reduceSource s2
reduceSource (NOT s      ) = 65535 - reduceSource s
reduceSource (LSHIFT s i ) = shiftL (reduceSource s) i
reduceSource (RSHIFT s i ) = shiftR (reduceSource s) i



-- build Source Tree from parsed map
buildSrc :: Map String String -> String -> Source
buildSrc kv s
    | "AND"    `isInfixOf` s = rawOrWire (delete s kv) (firstSrc s) `AND` buildSrc (delete s kv) (lastSrc s)
    | "OR"     `isInfixOf` s = rawOrWire (delete s kv) (firstSrc s) `OR` buildSrc (delete s kv) (lastSrc s)
    | "NOT"    `isInfixOf` s = NOT . buildSrc (delete s kv) $ lastSrc s
    | "LSHIFT" `isInfixOf` s = LSHIFT (buildSrc (delete s kv) $ firstSrc s) (read $ lastSrc s :: Int )
    | "RSHIFT" `isInfixOf` s = RSHIFT (buildSrc (delete s kv) $ firstSrc s) (read $ lastSrc s :: Int )
    | otherwise              = rawOrWire (delete s kv) s
-- helper fn for building source data
rawOrWire :: Map String String -> String -> Source
rawOrWire kv s = if all isDigit s then Raw (read s :: Int) else buildSrc kv s

-- helper functions
firstSrc :: String -> String
firstSrc = head . words
lastSrc :: String -> String
lastSrc = last . words



-- get input from file and parse it into Map of Destination -> Source
getInstructions :: FilePath -> IO (Map String String)
getInstructions fp = do
        Right instructions <- parseFromFile ( toKeyValue `sepEndBy` newline ) fp
        return $ fromList instructions

build :: String -> Map String String -> Source
build s kv = Wire s $ buildSrc kv s


main :: IO ()
main = do
    [f] <- getArgs
    is <- getInstructions f
    let src = build "a" is
    print src
