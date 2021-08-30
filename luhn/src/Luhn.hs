module Luhn (isValid) where

import Data.Char


doubleRule :: Int -> Int
doubleRule x = if doubled > 9 then doubled - 9 else doubled
  where doubled = 2 * x

doubleEachSecondDigit :: Char -> Int -> Char
doubleEachSecondDigit x i = if even (i+1) then head (show (doubleRule (digitToInt x))) else x

zipMap :: (a -> Int -> b) -> [a] -> [b]
zipMap f l = zipWith f l [0..]

isValidHelper :: String -> Bool
isValidHelper xs = mod summedDigits 10 == 0
  where
    newStr = zipMap doubleEachSecondDigit xs
    summedDigits = foldl (\acc x -> acc + digitToInt x) 0 newStr

isValid :: String -> Bool
isValid creditcard = (length processed > 1) && isValidHelper processed
  where processed = reverse (filter (/= ' ') creditcard)