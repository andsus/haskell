module DNA (toRNA) where

rnaMap :: Char -> Either Char Char
rnaMap 'G' = Right 'C'
rnaMap 'C' = Right 'G'
rnaMap 'T' = Right 'A'
rnaMap 'A' = Right 'U'
rnaMap other = Left other

toRNA :: String -> Either Char String
toRNA = mapM rnaMap
