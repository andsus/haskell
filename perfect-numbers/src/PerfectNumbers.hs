module PerfectNumbers (classify, Classification(..)) where

data Classification = Deficient 
  | Perfect 
  | Abundant deriving (Eq, Show)

classify :: Int -> Maybe Classification
classify number 
  | number <= 0 = Nothing
  | perfectNumber == number = Just Perfect
  | perfectNumber > number = Just Abundant
  | otherwise = Just Deficient
  where perfectNumber = sum(factors number)

factors :: Int -> [Int]
factors y = [x | x <- [1..y-1], y `mod` x == 0]