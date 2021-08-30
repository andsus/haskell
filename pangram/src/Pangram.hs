module Pangram (isPangram) where
import Data.Char
import qualified Data.Set as Set


alpha :: Set.Set Char
alpha = Set.fromList ['a'..'z'] 

isPangram :: String -> Bool
-- isPangram text = 
--     alpha `Set.isSubsetOf` Set.fromList [toLower a | a <- text]  
isPangram = any null . scanl deleteSet alpha
  where deleteSet cs c = Set.delete (toLower c) cs


