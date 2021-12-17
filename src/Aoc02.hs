module Aoc02
    ( solve02
    ) where

import System.IO
import Data.List

data SubmarineCommand 
    = Down Int
    | Up Int 
    | Forward Int
    | Invalid
    
data SubmarineState = SubmarineState { aim :: Int, horizontalPosition :: Int, depth :: Int }
    deriving Show
  
solve02 :: IO ()
solve02 = do
    handle <- openFile "assets/aoc02.txt" ReadMode
    contents <- hGetContents handle
    let inputValues = words <$> lines contents
    let SubmarineState { aim = _, horizontalPosition = h, depth = d } = executeChainOfCommands . parseCommands $ inputValues
    print (h * d)
    hClose handle
    
    
tidyUp :: [[String]] -> (Int,Int)
tidyUp inputLines =
    let wordPairs = Data.List.map (\ [command, dist] -> (command, dist)) inputLines
        getSpecificCommandAbs command = Data.List.sum . Data.List.map (read . snd) . Data.List.filter ((== command) . fst)
        forwardAbs = getSpecificCommandAbs "forward" wordPairs
        downAbs = getSpecificCommandAbs "down" wordPairs
        upAbs = getSpecificCommandAbs "up" wordPairs
    in (forwardAbs, downAbs - upAbs)
    
    
parseCommands :: [[String]] -> [SubmarineCommand]
parseCommands inputLines =
    let parsePair ("forward", numString) = Forward (read numString)
        parsePair ("down", numString) = Down (read numString)
        parsePair ("up", numString) = Up (read numString)
        parsePair _ = Invalid
        listToPair [command, dist] = (command, dist)
        listToPair _ = ("command", "0")
    in Data.List.map (parsePair . listToPair) inputLines

executeCommand :: SubmarineState -> SubmarineCommand -> SubmarineState
executeCommand SubmarineState { aim = a, horizontalPosition = h, depth = d } (Down x) =
    SubmarineState { aim = a + x, horizontalPosition = h, depth = d }
executeCommand SubmarineState { aim = a, horizontalPosition = h, depth = d } (Up x) =
    SubmarineState { aim = a - x, horizontalPosition = h, depth = d }
executeCommand SubmarineState { aim = a, horizontalPosition = h, depth = d } (Forward x) =
    SubmarineState { aim = a, horizontalPosition = h + x, depth = d + (a * x) }
executeCommand cmd Invalid = cmd

executeChainOfCommands :: Foldable t => t SubmarineCommand -> SubmarineState
executeChainOfCommands = 
    Data.List.foldl executeCommand SubmarineState {aim = 0, horizontalPosition = 0, depth = 0}