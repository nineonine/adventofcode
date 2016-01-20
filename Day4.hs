{-# LANGUAGE OverloadedStrings #-}

module Day4 where

import Data.ByteString as BS
import Data.String as S
import Crypto.Hash

md5 :: BS.ByteString -> Digest MD5
md5 = hash

toBS :: String -> BS.ByteString
toBS = S.fromString

hashAndTakeFirst :: Int -> String -> ByteString
hashAndTakeFirst i s = BS.take i . digestToHexByteString $ md5 (toBS s)

findMatching :: String -> [Int] -> Int
findMatching s (i:is) = if (hashAndTakeFirst 5 $ s ++ show i) == "00000" -- change first TAKEFIRST param and amt of zeroes in String to solve PART 2 
                        then i
                        else findMatching s is