module Pangram (isPangram) where

import Data.Char (toLower, isAlpha)
import Data.List (nub)

countLetters :: String -> Int
countLetters = length . nub . map toLower . filter isAlpha

isPangram :: String -> Bool
isPangram text = countLetters text == 26
