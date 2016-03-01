{-# LANGUAGE RankNTypes #-}

module Main where

allelves :: forall a. Integral a => a -> [a]
allelves n = lows ++ reverse (map (div n) lows)
    where lows = filter ((== 0) . mod n) [1..truncate . sqrt $ fromIntegral n]

removeFinishedElves :: [Int] -> [Int]
removeFinishedElves []        = []
removeFinishedElves [x]       = [x]
removeFinishedElves a@(x:xs)
    | div (last xs) x > 50 = removeFinishedElves xs
    | otherwise            = a

housesWithPresentsP1 :: [(Int, Int)] -- stream of (House Number, Number of presents)
housesWithPresentsP1 = zip [1..] (map ((*10) . sum . allelves) [1..]) 

housesWithPresentsP2 :: [(Int, Int)] -- stream of (House Number, Number of presents)
housesWithPresentsP2 = zip [1..] (map ((*11) . sum . removeFinishedElves . allelves) [1..])

firstHouseOf :: [(Int, Int)] -> Int
firstHouseOf = fst . head . dropWhile ((<29000000) . snd)

main :: IO ()
main = do
    print $ firstHouseOf housesWithPresentsP1
    print $ firstHouseOf housesWithPresentsP2