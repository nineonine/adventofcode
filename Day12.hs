{-# LANGUAGE OverloadedStrings, FlexibleContexts, NoMonomorphismRestriction, RankNTypes, KindSignatures #-}

module Day12 where

import Control.Lens
import Data.Aeson
import Data.Aeson.Lens
import Data.Scientific
import Data.Char(isDigit)
import Text.Parsec hiding (getInput)
import qualified Data.ByteString.Lazy as LB

integer :: Stream s m Char => ParsecT s u m Integer
integer = rd <$> (minus <|> number)
    where rd     = read :: String -> Integer
          minus  = (:) <$> char '-' <*> number
          number = many1 digit

noise :: forall s u (m :: * -> *). Stream s m Char => ParsecT s u m String 
noise = many1 (satisfy (\x -> not (isDigit x || x == '-')))

allIntegers :: forall s u (m :: * -> *). Stream s m Char => ParsecT s u m [Integer]
allIntegers = noise *> sepEndBy integer (many noise)

getInput :: IO LB.ByteString
getInput = LB.readFile "day12input.txt"


traverseJSON :: Value -> Int
traverseJSON  (Number n)   = fromIntegral $ coefficient n :: Int  
traverseJSON a@(Array _)   = sum . map traverseJSON $ a ^.. values
traverseJSON o@(Object _)  = if hasRedString o
                                then 0
                                else sum . map (traverseJSON . snd) $ o ^@.. members
traverseJSON _             = 0

hasRedString :: Value -> Bool
hasRedString o@(Object _) = any isRed vals
                        where 
                        vals = map snd $ o ^@.. members
                        isRed (String s) = s == "red"
                        isRed _ = False
hasRedString _ = False

main :: IO ()
main = do
    inp <- getInput
    let Just jsn = decode inp :: Maybe Value
    let Right r = parse allIntegers "day12" inp
    print $ sum r -- part 1
    print $ traverseJSON jsn -- part 2