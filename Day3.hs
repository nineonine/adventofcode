{-# LANGUAGE OverloadedStrings #-}

module Day3 where

import Control.Monad.State.Lazy
import Data.List

-- PART 1

-- datatype representing house 
-- with x and y coordinates and status of present delivery
data House = House {
      x :: Int
    , y :: Int
} deriving (Show, Eq)

-- type synonym for the sequence of moves
type Route = String

-- type synonym for State Monad 
type SantaLog = State [House] 

-- function for obtaining next house using direction symbol
nextHouse :: Char -> House -> House
nextHouse '^' h = h { y = y h + 1}
nextHouse 'v' h = h { y = y h - 1}
nextHouse '<' h = h { x = x h - 1}
nextHouse '>' h = h { x = x h + 1}

-- recursive function for running through the route
santaDelivery :: Route -> SantaLog ()
santaDelivery ""     = return ()
santaDelivery (r:rs) = do
     prev <- gets head
     next <- return $ nextHouse r prev
     modify $ (:) next
     santaDelivery rs

-- PART 2

-- we just create the same recursive function but this time we parse two direction
-- instructions at once 
twoSantasDelivery :: Route -> SantaLog ()
twoSantasDelivery ""         = return ()
twoSantasDelivery (r1:r2:rs) = do
    prevSantaHouse <- gets ( head . reverse . take 2 )
    prevRoboSantaHouse <- gets head 
    nextSantaHouse <- return $ nextHouse r1 prevSantaHouse
    nextRoboHouse <- return $ nextHouse r2 prevRoboSantaHouse
    modify $ \s -> nextRoboHouse:nextSantaHouse:s
    twoSantasDelivery rs
 

-- wrapper for running our computation passing the route
-- substitute santaDelivery with twoSantasDelivery to solve part 2
numOfVisits :: Route -> [House]
numOfVisits r = execState ( santaDelivery r ) [House 0 0]

-- IO action where we read file with route -> collect path -> leave only unique houses and count them
countHouses :: FilePath -> IO Int
countHouses fp = do
    route <- readFile fp
    return . length . nub $ numOfVisits route



