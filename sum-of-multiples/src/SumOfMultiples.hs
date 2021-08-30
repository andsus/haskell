module SumOfMultiples (sumOfMultiples) where

import Data.Set (fromDistinctAscList, unions)

sumOfMultiples :: [Integer] -> Integer -> Integer
sumOfMultiples factors limit = sum bunched'
    where factors' = filter (/= 0) factors
          bunched' = unions [ fromDistinctAscList [f, 2*f..limit-1] | f <- factors']