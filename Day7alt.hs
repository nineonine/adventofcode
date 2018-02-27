{-# LANGUAGE ViewPatterns #-}

module Main where

import Data.Bits
import Data.Char
import Text.Parsec hiding (Empty)
import Text.Parsec.String
import qualified Data.Map as M

type W = String -- Wire
type Src = String -- Source
type ID = String
data Source = Wire ID Source
            | Value Int
            | And Source Source
            | Or Source Source
            | Not Source
            | Lshift Source Int
            | Rshift Source Int
            deriving (Show)

-- PARSERS --
toKeyValue :: Parsec String () (W, Src)
toKeyValue = flip (,) <$> manyTill anyChar (try (string " -> ")) <*> many1 letter

-- get input from file and parse it into Map of Destination -> Source
getInstructions :: IO (M.Map W Src) --
getInstructions = do
        Right instructions <- parseFromFile ( toKeyValue `sepEndBy` newline ) "day7input.txt"
        return $ M.fromList instructions

reduceSource :: Source -> Int
reduceSource (Wire _ src ) = reduceSource src
reduceSource (Value  int ) = int
reduceSource (s1 `And`s2 ) = reduceSource s1 .&. reduceSource s2
reduceSource (s1 `Or` s2 ) = reduceSource s1 .|. reduceSource s2
reduceSource (Not s      ) = 65535 - reduceSource s
reduceSource (Lshift s i ) = shiftL (reduceSource s) i
reduceSource (Rshift s i ) = shiftR (reduceSource s) i

parseSource :: M.Map W Src -> W -> Source
parseSource kv w = let kv' = M.delete w kv
                   in if all isDigit w
                       then Value (read w)
                       else case words (kv M.! w) of
                           ["NOT", id]       -> Not $ parseSource kv' id
                           [val]             -> if all isDigit val then Value (read val) else parseSource kv' val
                           [v1,"AND",v2]     -> parseSource kv' v1 `And` parseSource kv' v2
                           [v1,"OR",v2]      -> parseSource kv' v1 `Or` parseSource kv' v2
                           [v1, "LSHIFT", i] -> parseSource kv' v1 `Lshift` read i
                           [v1, "RSHIFT", i] -> parseSource kv' v1 `Rshift` read i


build :: String -> M.Map W Src -> Source
build s kv = Wire s $ parseSource kv s

main :: IO ()
main = do
    is <- getInstructions
    let src = build "a" is
    print src
