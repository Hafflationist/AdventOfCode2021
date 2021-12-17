module Aoc03
    ( solve03
    ) where

import System.IO
import Data.List as List
  
solve03 :: IO ()
solve03 = do
    handle <- openFile "assets/aoc03.txt" ReadMode
    content <- hGetContents handle
    let inputMatrix = getBinaryMatrix content
    print . uncurry (*) . lifeSupportRating $ inputMatrix
    hClose handle
    
    
getBinaryMatrix :: String -> [[Bool]]
getBinaryMatrix inputString =
    let charMatrix = lines inputString
    in fmap (fmap (== '1')) charMatrix
    
matrixToGamma :: [[Bool]] -> (Int, Int)
matrixToGamma matrix =
    let columns = List.transpose matrix
        oneVote = List.map (List.length . List.filter id) columns
        zeroVote = List.map (List.length . List.filter not) columns
        binaryGamma = List.zipWith (>) oneVote zeroVote
        binaryEpsilon = List.zipWith (<) oneVote zeroVote
    in (bin2dec binaryGamma, bin2dec binaryEpsilon)
    
bin2dec :: [Bool] -> Int
bin2dec bits =
    let folder acc False = acc * 2
        folder acc True = acc * 2 + 1
    in List.foldl folder 0 bits
    
lifeSupportRating :: [[Bool]] -> (Int, Int)
lifeSupportRating matrix =
    let bitLength = List.length . List.head $ matrix
        getMajority _ [row] _ = [row]
        getMajority op subMatrix nth =
            let oneVote = List.length . List.filter (!! nth) $ subMatrix
                zeroVote = List.length . List.filter (not . (!!nth)) $ subMatrix
            in List.filter (\ row -> row!!nth == (oneVote `op` zeroVote)) subMatrix
        [oxygenGeneratorRating] = List.foldl (getMajority (>=)) matrix [0..(bitLength - 1)]
        [co2ScrubberRating] = List.foldl (getMajority (<)) matrix [0..(bitLength - 1)]
    in (bin2dec oxygenGeneratorRating, bin2dec co2ScrubberRating)