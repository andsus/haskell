module Grains (square, total) where

square :: Integer -> Maybe Integer
square n
  | n > 64 || n < 1 = Nothing
  | n == 1 = Just 1
  | otherwise = Just (2 ^ (n - 1))

total :: Integer
total = 
    let x  = 63 :: Integer 
        in sum [2 ^ n | n <- [0 .. x]]



