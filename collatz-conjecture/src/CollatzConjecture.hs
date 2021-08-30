module CollatzConjecture (collatz) where

collatz :: Integer -> Maybe Integer
collatz number
  | number <= 0 = Nothing
  | otherwise = Just (collatz' number 0)
  where
    collatz' 1 acc = acc -- collatz' infinite list
    collatz' n acc
      | even n = collatz' (n `div` 2) (acc + 1)
      | otherwise = collatz' (n * 3 + 1) (acc + 1)