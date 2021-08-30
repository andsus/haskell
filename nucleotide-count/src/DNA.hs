module DNA (nucleotideCounts, Nucleotide(..)) where

import Data.Map (Map, fromList, adjust)
import Control.Monad (foldM)


data Nucleotide = A | C | G | T deriving (Eq, Ord, Show)

type NucleotideCount = Map Nucleotide Int

mapNucleotide :: Char -> Either String Nucleotide
mapNucleotide 'A' = Right A
mapNucleotide 'C' = Right C
mapNucleotide 'G' = Right G
mapNucleotide 'T' = Right T
mapNucleotide  other = Left (other : " is invalid nucleotide")


nucleotideCounts :: String -> Either String (Map Nucleotide Int)
nucleotideCounts = 
    foldM incrementNucleotide (fromList [(A, 0), (C, 0), (G, 0), (T, 0)])
  where
    incrementNucleotide ::
         NucleotideCount -> Char -> Either String NucleotideCount
    incrementNucleotide count char =
      fmap (\nucleotide -> adjust (+ 1) nucleotide count) (mapNucleotide char)
