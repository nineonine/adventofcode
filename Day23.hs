{-# LANGUAGE LambdaCase #-}

module Day23 where

import Control.Monad
import Control.Monad.State.Lazy
import qualified Data.Map.Lazy as M

getInput :: IO [[String]]
getInput = fmap words . lines <$> readFile "day23input.txt"

type Offset = Int
type RegisterMap = M.Map Char Double
type InstructionList = [(Int, Instruction)]
type Program = State ProgState
data ProgState = PS { ilist :: InstructionList, regMap :: RegisterMap, prog :: [Instruction] } deriving (Show)
data Instruction = Hlf Char
                 | Tpl Char
                 | Inc Char
                 | Jmp Offset
                 | Jie Char Offset
                 | Jio Char Offset
                 deriving (Show)

parseInt :: String -> Int
parseInt = \case
    ('-':rest) -> negate $ read rest
    ('+':rest) -> read rest

parseInstruction :: [String] -> Instruction
parseInstruction = \case
    a@("hlf":_) -> Hlf . head $ last a
    a@("tpl":_) -> Tpl . head $ last a
    a@("inc":_) -> Inc . head $ last a
    a@("jmp":_) -> Jmp ( parseInt $ last a )
    a@("jie":_) -> Jie (head . head $ drop 1 a) ( parseInt $ last a )
    a@("jio":_) -> Jio (head . head $ drop 1 a) ( parseInt $ last a )

buildProgram :: Int -> Program () -- [Instruction]
buildProgram n = do
    PS il rm acc <- get
    case lookup n il of
        Nothing -> return ()
        Just (Hlf c) -> pushInstruction (Hlf c) >> modify' (half c) >> buildProgram (n+1)
        Just (Tpl c) -> pushInstruction (Tpl c) >> modify' (triple c) >> buildProgram (n+1)
        Just (Inc c) -> pushInstruction (Inc c) >> modify' (inc c) >> buildProgram (n+1)
        Just (Jmp o) -> buildProgram (n+o)
        Just (Jie c o) -> let val = rm M.! c
                          in if round val `mod` 2 == 0
                              then buildProgram (n+o)
                              else buildProgram (n+1)
        Just (Jio c o) -> let val = rm M.! c
                          in if val == 1
                             then buildProgram (n+o)
                             else buildProgram (n+1)

pushInstruction :: Instruction -> Program ()
pushInstruction i = do
    PS il rm acc <- get
    let acc' = i:acc
    put (PS il rm acc')

half :: Char -> ProgState -> ProgState
half c (PS il rm is) = let rm' = M.adjust (/2) c rm
                       in PS il rm' is

triple :: Char -> ProgState -> ProgState
triple c (PS il rm is) = let rm' = M.adjust (*3) c rm
                         in PS il rm' is

inc :: Char -> ProgState -> ProgState
inc c (PS il rm is) = let rm' = M.adjust (+1) c rm
                      in PS il rm' is

main :: IO ()
main = do
    input <- map parseInstruction <$> getInput
    let insList = zip [1..] input
        -- rMap = M.fromList [('a',0), ('b',0)] -- part 1
        rMap = M.fromList [('a',1), ('b',0)]
        initialState = PS insList rMap []
        PS il rm is = execState (buildProgram 1) initialState
        b = rm M.! 'b'
    print b
