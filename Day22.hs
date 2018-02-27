module Day22 where

-- import D22types

data RoundState = GS { effs :: [Effect], player :: Player, boss :: Player } deriving (Show)

type Round m = Monad m => StateT m