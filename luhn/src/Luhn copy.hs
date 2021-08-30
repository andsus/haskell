module Luhn (isValid) where

import Data.Char
--https://codereview.stackexchange.com/questions/226748/introduction-to-haskell-validating-credit-card-numbers
-- toDigits :: Integer -> [Integer]
-- toDigits = eval 
--   where eval xs = foldl (\x y -> y + (10 * x)) 0 xs

-- toDigitsRev :: Integer -> [Integer]
-- toDigits = evalRev
--   where evalRev xs = foldr (\x y -> x + (10 * y)) 0 xs


doubleRule :: Int -> Int
doubleRule x = if doubled > 9 then doubled - 9 else doubled
  where doubled = 2 * x

zipMap :: (a -> Int -> b) -> [a] -> [b]
zipMap f l = zipWith f l [0..]

doubleEachSecondDigit :: Char -> Int -> Char
doubleEachSecondDigit x i = if even (i+1) then head (show (doubleRule (digitToInt x))) else x

isValidHelper :: String -> Bool
isValidHelper xs = mod summedDigits 10 == 0
  where
    newStr = zipMap doubleEachSecondDigit xs
    summedDigits = foldl (\acc x -> acc + digitToInt x) 0 newStr

isValid :: String -> Bool
isValid creditcard = (length processed > 1) && isValidHelper processed
  where processed = reverse (filter (/= ' ') creditcard)