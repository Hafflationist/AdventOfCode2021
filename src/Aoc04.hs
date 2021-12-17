module Aoc04
    ( solve04
    ) where

import System.IO
import Data.Char as Char
import Data.List as List
import Data.List.Split
import Data.Set as Set
import Data.Text
  
  
type Board = [[Int]]

  
solve04 :: IO ()
solve04 = do
    handle <- openFile "assets/aoc04.txt" ReadMode
    content <- hGetContents handle
    print
        . uncurry simulateBingoAndGetWinnerScore
        $ parseLinesToBoards content
    print
        . uncurry simulateBingoAndGetLoserScore
        $ parseLinesToBoards content
    hClose handle
    
    
parseBoard :: Functor f => f String -> f [Int]
parseBoard = fmap (List.map read . Prelude.words)


parseLinesToBoards :: String -> ([Int], [Board])
parseLinesToBoards inputString =
    let headLine : inputLines = Prelude.lines inputString
        drawnNumbers :: [Int]
        drawnNumbers = read 
                     . Data.Text.unpack 
                   <$> Data.Text.splitOn (Data.Text.pack ",") (Data.Text.pack headLine)
        boards :: [Board]
        boards = fmap parseBoard
               . List.filter (not . Prelude.null) 
               . Data.List.Split.splitWhen (Prelude.null . Prelude.dropWhile Char.isSpace) 
               $ inputLines
    in (drawnNumbers, boards)
    

isBoardWinner :: [Int] -> Board -> Bool
isBoardWinner drawnNumbers board =
    let checkLine :: [Int] -> Bool 
        checkLine = List.all (`elem` drawnNumbers)
        horz = List.any checkLine board
        vert = List.any checkLine . List.transpose $ board
    in horz || vert
    
    
calcScore :: [Int] -> Board -> Int
calcScore drawnNumbers board =
    let lastDrawnNumber = List.last drawnNumbers
        preMultiplicationScore = 
            List.sum
          . List.filter (not . (`elem` drawnNumbers)) 
          . Prelude.concat 
          $ board
    in lastDrawnNumber * preMultiplicationScore


simulateBingoAndGetWinnerScore :: [Int] -> [Board] -> Maybe Int
simulateBingoAndGetWinnerScore drawnNumbers boards =
    let numbersSubsetWithFirstWinner = 
            List.head
          . List.dropWhile (\ drawnNumberSubset -> not . List.any (isBoardWinner drawnNumberSubset) $ boards)
          . List.map (`List.take` drawnNumbers)
          $ [0..(Prelude.length drawnNumbers)]
        winnerBoard = List.find (isBoardWinner numbersSubsetWithFirstWinner) boards
    in calcScore numbersSubsetWithFirstWinner <$> winnerBoard


simulateBingoAndGetLoserScore :: [Int] -> [Board] -> Maybe Int
simulateBingoAndGetLoserScore drawnNumbers boards =
    let numbersSubsetWithFirstLoser = 
            List.head
          . List.dropWhile (\ drawnNumberSubset -> List.all (isBoardWinner drawnNumberSubset) boards)
          . List.map (`List.take` drawnNumbers)
          . List.reverse
          $ [0..(Prelude.length drawnNumbers)]
        loserBoard = List.find (not . isBoardWinner numbersSubsetWithFirstLoser) boards
        numbersSubsetWithLastWinner = List.take (List.length numbersSubsetWithFirstLoser + 1) drawnNumbers
    in calcScore numbersSubsetWithLastWinner <$> loserBoard