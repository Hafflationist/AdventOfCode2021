{-# LANGUAGE TupleSections #-}

module Lib
    ( solve02
    ) where

import System.IO
import Data.Char as Char
import Data.Bifunctor as Bifunctor
import Data.List as List hiding ((\\))
import Data.List.Split
import Data.Maybe as Maybe
import Data.Text as Text

import Debug.Trace


data InsertionRule = InsertionRule { educt1 :: Char, educt2 :: Char, addend :: Char }
    deriving Show
    
type Polymer = [(Int, Char, Char)]


solve02 :: IO ()
solve02 = do
    handle <- openFile "assets/aoc14.txt" ReadMode
    content <- hGetContents handle
    let (insertionRule, initPolymer) = parsePolymerFactory content
    print
      . getScore
      . executeNSteps (expandPolymer insertionRule) 10
      $ initPolymer
    print
      . getScore
      . executeNSteps (expandPolymer insertionRule) 40
      $ initPolymer
    hClose handle
    

parsePolymerTemplate :: String -> Polymer
parsePolymerTemplate inputString = 
    let uncompressedPolymer = List.head . Prelude.lines $ inputString
        uncountedPolymer :: [(Char, Char)]
        uncountedPolymer = List.zip uncompressedPolymer (List.tail uncompressedPolymer)
    in List.map (\ (e1, e2) -> (List.length . List.filter (==(e1, e2)) $ uncountedPolymer, e1, e2))
            . List.nub 
            $ uncountedPolymer


parseInsertionRules :: String -> [InsertionRule]
parseInsertionRules inputString =
    let inputLines = List.tail . List.tail . Prelude.lines $ inputString
        splitLine line = Text.unpack <$> Text.splitOn (Text.pack " -> ") (Text.pack line)
        parseSplit [[e1, e2], [a]] = InsertionRule { educt1 = e1, educt2 = e2, addend = a } 
    in List.map (parseSplit . splitLine) inputLines
    
    
parsePolymerFactory :: String -> ([InsertionRule], Polymer)
parsePolymerFactory inputString = (parseInsertionRules inputString, parsePolymerTemplate inputString)


calcAddend :: [InsertionRule] -> Char -> Char -> Char
calcAddend insertionRules e1 e2 =
    addend 
  . List.head
  . List.filter (\ InsertionRule { educt1 = ed1, educt2 = ed2, addend = ad } -> ed1 == e1 && ed2 == e2) 
  $ insertionRules

-- TODO: Überarbeitung der folgenden Funktionen. (Nutzung komprimierter Polymere)
expandPolymer :: [InsertionRule] -> Polymer -> Polymer
expandPolymer insertionRules polymer =
    let pairwisePolymer = List.zip polymer (List.tail polymer)
        newPolymerTail = List.concatMap (\ (e1, e2) ->  [calcAddend insertionRules e1 e2, e2]) pairwisePolymer
    in List.head polymer : newPolymerTail
    
    
executeNSteps :: (a -> a) -> Int -> a -> a
executeNSteps func n = List.foldr (.) id (Prelude.replicate n func)


getScore :: String -> Int
getScore polymer =
    let countings = List.map (\ character -> List.length . List.filter (==character) $ polymer)
                     . List.nub 
                     $ polymer
    in List.maximum countings - List.minimum countings