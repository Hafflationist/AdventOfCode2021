{-# LANGUAGE TupleSections #-}

module Aoc05
    ( solve05
    ) where

import System.IO
import Data.List as List
import Data.Set as Set
import Data.Text
  
  
solve05 :: IO ()
solve05 = do
    handle <- openFile "assets/aoc05.txt" ReadMode
    content <- hGetContents handle
    print 
        . getNumberOfPointsWhichOccurAtLeast2Times 
        . Prelude.concat 
        $ (pointToLine . parseLine <$> Prelude.lines content)
    hClose handle
    
    
parseLine :: String -> (Int, Int, Int, Int)
parseLine inputLine =
    let [firstPair, _, secondPair] = Prelude.words inputLine
        parsePair pairString = 
            let [x, y] = Data.Text.unpack <$> Data.Text.splitOn (Data.Text.pack ",") (Data.Text.pack pairString)
            in (read x, read y)
        (x1, y1) = parsePair firstPair
        (x2, y2) = parsePair secondPair
    in (x1, y1, x2, y2)
    
    
pointToLine :: (Int, Int, Int, Int) -> [(Int, Int)]
pointToLine (x1, y1, x2, y2) 
    | x1 == x2 = fmap (x1,) [(min y1 y2) .. (max y1 y2)]
    | y1 == y2 = fmap (,y1) [(min x1 x2) .. (max x1 x2)]
    | otherwise =
        let getRange a b = (if a < b then id else List.reverse) [(min a b) .. (max a b)]
        in List.zip (getRange x1 x2) (getRange y1 y2)

  
getNumberOfPointsWhichOccurAtLeast2Times :: [(Int, Int)] -> Int
getNumberOfPointsWhichOccurAtLeast2Times points =
    let pointsSet = Set.fromList points
    in Set.size 
       . Set.filter (\ point -> (2 <=) . List.length . List.filter (point == ) $ points)
       $ pointsSet
