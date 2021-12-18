module Lib
    ( solve14
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
    
    
data Polymer = Polymer { pairs :: [(Int, Char, Char)], counts :: [(Char, Int)] }
    deriving Show


solve14 :: IO ()
solve14 = do
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
        compressedPairs = List.map (\ (e1, e2) -> (List.length . List.filter (==(e1, e2)) $ uncountedPolymer, e1, e2))
                        . List.nub 
                        $ uncountedPolymer
        countings = List.map (\ character -> (character, List.length . List.filter (==character) $ uncompressedPolymer))
                  . List.nub 
                  $ uncompressedPolymer
    in Polymer { pairs = compressedPairs, counts = countings }


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
  . List.filter ((e1 ==) . educt1)
  . List.filter ((e2 ==) . educt2)
  $ insertionRules


compressPairs :: [(Int, Char, Char)] -> [(Int, Char, Char)]
compressPairs semiCompressedPairs =
    let pairsSet = List.nub . List.map (\ (_, e1, e2) -> (e1, e2)) $ semiCompressedPairs
        calcNForPair (e1, e2) = sum 
                              . List.map (\ (n, _, _) -> n) 
                              . List.filter (\ (_, el1, el2) -> e1 == el1 && e2 == el2)
                              $ semiCompressedPairs
    in List.map (\ (e1, e2) -> (calcNForPair (e1, e2), e1, e2)) pairsSet


expandPolymer :: [InsertionRule] -> Polymer -> Polymer
expandPolymer insertionRules Polymer { pairs = ps, counts = cs } =
    let expandPair (n, e1, e3) =
            [(n, e1, e2), (n, e2, e3)]
            where e2 = calcAddend insertionRules e1 e3
        newPairs = compressPairs . List.concatMap expandPair $ ps
        allChars = List.nub . List.concatMap (\ (_, e1, e2) -> [e1, e2]) $ newPairs
        getNumOfAddedChar newChar =
            sum . List.map (\ (n, e1, e2) -> if newChar == calcAddend insertionRules e1 e2 then n else 0) $ ps
        newCounts = List.map (\ c -> (c, getNumOfAddedChar c + (sum . List.map snd . List.filter ((==c) . fst) $ cs))) allChars
    in Polymer { pairs = newPairs, counts = newCounts }
    
    
executeNSteps :: (a -> a) -> Int -> a -> a
executeNSteps func n = List.foldr (.) id (Prelude.replicate n func)


getScore :: Polymer -> Int
getScore Polymer { counts = cs } =
    let countings = List.map snd cs
    in List.maximum countings - List.minimum countings