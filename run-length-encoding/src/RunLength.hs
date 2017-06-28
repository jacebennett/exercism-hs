module RunLength (decode, encode, expand, tokenize) where

import Data.List (group, groupBy)
import Data.Char (isDigit)

isRun :: Char -> Char -> Bool
isRun s t
  | not $ isDigit s = s == t
  | otherwise = isDigit s == isDigit t

tokenize :: String -> [String]
tokenize = groupBy isRun

expand :: [String] -> [String]
expand [] = []
expand [c] = [c]
expand (n:c:rest)
  | not $ isDigit $ head n = n : expand (c:rest)
  | otherwise        = concat (replicate (read n :: Int) c) : expand rest


decode :: String -> String
decode = concat . expand . tokenize


run :: String -> [String]
run s
  | length s == 1 = [s]
  | otherwise     = [show $ length s, [head s]]

encode :: String -> String
encode = concat . concatMap run . group
