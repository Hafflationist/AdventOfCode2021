module Aoc10
    ( solve10
    ) where

import System.IO
import Data.Char as Char
import Data.List as List
import Data.Maybe as Maybe
import Data.Text


solve10 :: IO ()
solve10 = do
    handle <- openFile "assets/aoc10.txt" ReadMode
    content <- hGetContents handle
    print 
      . List.sum 
      . List.map characterToPoints
      . Maybe.mapMaybe analyzeCorruptedLine
      . Prelude.lines
      $ content
    print 
      . middle
      . List.sort 
      . List.map completionScore
      . Maybe.mapMaybe analyzeIncompleteLine
      . Prelude.lines
      $ content
    hClose handle
    
    
middle :: [a] -> [a]
middle xs = List.take (signum ((l + 1) `mod` 2) + 1) $ List.drop ((l - 1) `div ` 2) xs
    where l = List.length xs


isClosing :: Char -> Bool
isClosing ')' = True
isClosing ']' = True
isClosing '}' = True
isClosing '>' = True
isClosing _ = False


openClose :: Char -> Char -> Bool
openClose '(' ')' = True
openClose '[' ']' = True
openClose '{' '}' = True
openClose '<' '>' = True
openClose _ _ = False


analyzeCorruptedLine :: [Char] -> Maybe Char
analyzeCorruptedLine inputLine =
    let step :: [Char] -> [Char] -> Maybe Char
        step _ [] = Nothing
        step [] (nextCharacter : lineTail) = step [nextCharacter] lineTail
        step (currentCharacter : stackTail) (nextCharacter : lineTail) 
            | isClosing nextCharacter && not (openClose currentCharacter nextCharacter) = Just nextCharacter
            | isClosing nextCharacter && openClose currentCharacter nextCharacter = step stackTail lineTail
            | otherwise = step (nextCharacter : currentCharacter : stackTail) lineTail
    in step [] inputLine
    
    
characterToPoints :: Char -> Int
characterToPoints ')' = 3
characterToPoints ']' = 57
characterToPoints '}' = 1197
characterToPoints '>' = 25137
characterToPoints _ = 0
  
  
openToClose :: Char -> Char
openToClose '(' = ')'
openToClose '[' = ']'
openToClose '{' = '}'
openToClose '<' = '>'
openToClose _ = '\0'


analyzeIncompleteLine :: [Char] -> Maybe [Char]
analyzeIncompleteLine inputLine =
    let step :: [Char] -> [Char] -> Maybe [Char]
        step notClosed' [] = Just notClosed'
        step [] (nextCharacter : lineTail) = step [nextCharacter] lineTail
        step (currentCharacter : stackTail) (nextCharacter : lineTail) 
            | isClosing nextCharacter && not (openClose currentCharacter nextCharacter) = Nothing
            | isClosing nextCharacter && openClose currentCharacter nextCharacter = step stackTail lineTail
            | otherwise = step (nextCharacter : currentCharacter : stackTail) lineTail
        notClosed = step [] inputLine
    in fmap openToClose <$> notClosed
    

partialCompletionScore :: Char -> Int
partialCompletionScore ')' = 1
partialCompletionScore ']' = 2
partialCompletionScore '}' = 3
partialCompletionScore '>' = 4
partialCompletionScore _ = 0
    
    
completionScore :: [Char] -> Int
completionScore = List.foldl (\ score character -> score * 5 + partialCompletionScore character) 0