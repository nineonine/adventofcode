module Day1 where

import Data.Monoid

-- Part One

-- using recursion	
whatfloorRec :: [Char] -> Int
whatfloorRec ('(':rest) =     (+1)     $ whatfloorRec rest 
whatfloorRec (')':rest) = (subtract 1) $ whatfloorRec rest 
whatfloorRec    []      = 0

-- using Monoids
whatfloor2 :: [Char] -> Sum Int
whatfloor2 = mconcat . map (Sum . procAll) where
	procAll '(' = 1
	procAll ')' = -1


-- Part Two
basementEnterPosition :: [Char] -> Int
basementEnterPosition  ""    = 0
basementEnterPosition instrs = let indexedList = zip instrs [1..]         -- create indexed assoc list
							   in go (0, indexedList)					  -- go through everything
							   where go (0, (')', i):_ ) = i             
							         go (a, ('(', _):xs) = go (a+1, xs)
							         go (a, (')', _):xs) = go (a-1, xs)	
							         go _                = 0
