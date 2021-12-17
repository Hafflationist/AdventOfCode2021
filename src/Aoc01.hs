module Aoc01
    ( solve01
    ) where

import System.IO
import Data.List
  
solve01 :: IO ()
solve01 = do
    handle <- openFile "assets/aoc01.txt" ReadMode
    contents <- hGetContents handle
    let inputValues = read <$> lines contents :: [Int]
    print . countIncreases . blurList $ inputValues 
    hClose handle
    
blurList :: [Int] -> [Int]
blurList values =
    Data.List.map (\ idx -> values!!(idx - 1) + values!!idx + values!!(idx + 1)) 
    . Data.List.init 
    . Data.List.tail 
    $ [0..(Data.List.length values - 1)] 
    
countIncreases :: [Int] -> Int
countIncreases values =
    let pres = Data.List.tail values
        curs = Data.List.init values
    in 
        Data.List.length
        . Data.List.filter (>0)
        . Data.List.zipWith (-) pres
        $ curs
