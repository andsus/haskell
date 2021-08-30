module LeapYear (isLeapYear) where

isDivBy :: Integer -> Integer -> Bool
isDivBy a b = a `rem` b == 0

isLeapYear :: Integer -> Bool
isLeapYear year = 
    isDivBy year 4 &&
    (not
        (isDivBy year 100) ||
        isDivBy year 400
    )    