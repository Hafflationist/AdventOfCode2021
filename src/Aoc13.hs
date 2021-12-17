{-# LANGUAGE TupleSections #-}

module Lib
    ( solve13
    ) where

import System.IO
import Data.Char as Char
import Data.Bifunctor as Bifunctor
import Data.List as List hiding ((\\))
import Data.List.Split
import Data.Maybe as Maybe
import Data.Text as Text

import Debug.Trace


data FoldInstruction
    = VerticalFold { x :: Int }
    | HorizontalFold { y :: Int }
    | InvalidFold
    deriving Show


solve13 :: IO ()
solve13 = do
    handle <- openFile "assets/aoc13.txt" ReadMode
    content <- hGetContents handle
    let manual = parseManual content
    print 
      . List.length 
      . fst 
      . executeFoldInstruction 
      $ manual
    putStrLn 
      . visualizeDots 
      . executeAllFoldInstruction 
      $ manual
    hClose handle
    

parseDots :: String -> [(Int, Int)]
parseDots inputString =
    let inputLines = List.takeWhile (not . List.null) . Prelude.lines $ inputString
        parseLine line = Text.unpack <$> Text.splitOn (Text.pack ",") (Text.pack line)
    in List.map ((\ [x, y] -> (x, y)) . fmap read . parseLine) inputLines


parseInstruction :: String -> [FoldInstruction]
parseInstruction inputString =
    let (_: inputLines) = List.dropWhile (not . List.null) . Prelude.lines $ inputString
        splitLine line = Text.unpack <$> Text.splitOn (Text.pack "=") (Text.pack line)
        parseSplit ["fold along y", yValue] = HorizontalFold { y = read yValue } 
        parseSplit ["fold along x", xValue] = VerticalFold { x = read xValue }
        parseSplit _ = InvalidFold
    in List.map (parseSplit . splitLine) inputLines
    
    
parseManual :: String -> ([(Int, Int)], [FoldInstruction])
parseManual inputString = (parseDots inputString, parseInstruction inputString)


executeFoldInstruction :: ([(Int, Int)], [FoldInstruction]) -> ([(Int, Int)], [FoldInstruction])
executeFoldInstruction (dots, []) = (dots, []) 
executeFoldInstruction (dots, InvalidFold : instructionsTail) = (dots, instructionsTail) 
executeFoldInstruction (dots, VerticalFold { x = xLine } : instructionsTail) =
    let topDots = List.filter ((xLine >) . fst) dots
        foldedDots = List.map (Bifunctor.first (\ xVal -> xLine * 2 - xVal)) . List.filter (`notElem` topDots) $ dots
    in (topDots `union` foldedDots, instructionsTail)
executeFoldInstruction (dots, HorizontalFold { y = yLine } : instructionsTail) =
    let topDots = List.filter ((yLine >) . snd) dots
        foldedDots = List.map (Bifunctor.second (\ yVal -> yLine * 2 - yVal)) . List.filter (`notElem` topDots) $ dots
    in (topDots `union` foldedDots, instructionsTail)
    
    
executeAllFoldInstruction :: ([(Int, Int)], [FoldInstruction]) -> [(Int, Int)]
executeAllFoldInstruction (dots, instructions) = 
    let eafiInner (ds, []) = (ds, [])
        eafiInner (ds, is) = eafiInner . executeFoldInstruction $ (ds, is)
    in fst . eafiInner $ (dots, instructions)
    
    
visualizeDots :: [(Int, Int)] -> String
visualizeDots dots =
    let allXValues = [0..(List.maximum . List.map fst $ dots)]
        allYValues = [0..(List.maximum . List.map snd $ dots)]
        getChar a b = if (a, b) `elem` dots then '#' else '.'
    in List.intercalate "\n" [[getChar a b | a <- allXValues] | b <- allYValues]