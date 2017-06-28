module Bob (responseFor) where

import Data.Char (toLower, toUpper, isSpace)
import Data.List (dropWhileEnd)

responseFor :: String -> String
responseFor xs
  | null xs
    || all isSpace xs                       = "Fine. Be that way!"
  | xs == map toUpper xs
    && xs /= map toLower xs                 = "Whoa, chill out!"
  | last (dropWhileEnd isSpace xs) == '?'   = "Sure."
  | otherwise                               = "Whatever."
