module Aoc06
    ( solve06
    ) where

import System.IO
import Data.Char as Char
import Data.List as List
import Data.List.Split
import Data.Set as Set
import Data.Text
  

data LanternfishPopulation = LanternfishPopulation { 
    is0 :: Int, is1 :: Int, is2 :: Int, is3 :: Int, is4 :: Int, is5 :: Int, is6 :: Int, is7 :: Int, is8 :: Int
}

  
solve06 :: IO ()
solve06 = do
    handle <- openFile "assets/aoc06.txt" ReadMode
    content <- hGetContents handle
    print
        . countFishes
        . executeNSteps 256
        $ parseInitState content
    hClose handle
    
    
parseInitState :: String -> LanternfishPopulation
parseInitState inputString = 
    let internalStates = read 
                       . Data.Text.unpack 
                     <$> Data.Text.splitOn (Data.Text.pack ",") (Data.Text.pack inputString)
        countOccurrences n = List.length . List.filter (== n) $ internalStates
    in LanternfishPopulation { 
          is0 = countOccurrences 0, 
          is1 = countOccurrences 1, 
          is2 = countOccurrences 2, 
          is3 = countOccurrences 3, 
          is4 = countOccurrences 4, 
          is5 = countOccurrences 5, 
          is6 = countOccurrences 6, 
          is7 = countOccurrences 7, 
          is8 = countOccurrences 8  
     }

nextStep :: LanternfishPopulation -> LanternfishPopulation
nextStep LanternfishPopulation { 
   is0 = i0, is1 = i1, is2 = i2, is3 = i3, is4 = i4, is5 = i5, is6 = i6, is7 = i7, is8 = i8  
} = LanternfishPopulation { is0 = i1, is1 = i2, is2 = i3, is3 = i4, is4 = i5, is5 = i6, is6 = i7 + i0, is7 = i8, is8 = i0 }
    
    
executeNSteps :: Int -> LanternfishPopulation -> LanternfishPopulation
executeNSteps n = List.foldr (.) id (Prelude.replicate n nextStep)


countFishes :: LanternfishPopulation -> Int
countFishes LanternfishPopulation { 
  is0 = i0, is1 = i1, is2 = i2, is3 = i3, is4 = i4, is5 = i5, is6 = i6, is7 = i7, is8 = i8  
} = i0 + i1 + i2 + i3 + i4 + i5 + i6 + i7 + i8