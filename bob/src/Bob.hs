module Bob (responseFor) where

import Data.Char


responseFor :: String -> String
responseFor utterance
  | isQuiet = "Fine. Be that way!"
  | isShouting && isAsking = "Calm down, I know what I'm doing!"
  | isShouting =  "Whoa, chill out!"
  | isAsking = "Sure."
  | otherwise = "Whatever."
  where
    trimSpace = filter (not . isSpace) utterance
    charOnly = filter isLetter trimSpace
    isQuiet = null trimSpace
    isShouting = (not . null) charOnly && all isUpper charOnly
    isAsking = not isQuiet && (last trimSpace == '?')