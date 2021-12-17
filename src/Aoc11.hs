{-# LANGUAGE TupleSections #-}

module Aoc11
    ( solve11
    ) where

import System.IO
import Data.Char as Char
import Data.Bifunctor as Bifunctor
import Data.List as List hiding ((\\))
import Data.Maybe as Maybe
import Data.Text
  
  
type EnergyLevelMap = [[Int]]


solve11 :: IO ()
solve11 = do
    handle <- openFile "assets/aoc11.txt" ReadMode
    content <- hGetContents handle
    let initEml = parseEnergyLevels content
    print
      . snd
      . executeNSteps 100
      . (, 0)
      $ initEml
    print 
      . List.length
      . List.takeWhile (not . checkSynchronization . fst)
      . List.map (`executeNSteps` (initEml, 0))
      $ [0..]
    hClose handle
    
parseEnergyLevels :: String -> EnergyLevelMap
parseEnergyLevels  = fmap (fmap (\ c -> read [c])) . Prelude.lines


getValidNeighbourhood :: EnergyLevelMap -> (Int, Int) -> [(Int, Int)]
getValidNeighbourhood energyLevelMap (x, y) =
    let isValidCoord (xi, yi) = 0 <= xi && xi < List.length energyLevelMap 
                              && 0 <= yi && yi < (List.length . Prelude.head $ energyLevelMap)
    in List.filter isValidCoord coords
    where coords = [(x + dx, y + dy) | dx <- [-1..1], dy <- [-1..1], dx /= 0 || dy /= 0]


indexed :: [[a]] -> [(Int, [(Int, a)])]
indexed = List.zipWith (\ ix xrow -> (ix, List.zip [0..] xrow)) [0..]


step1 :: EnergyLevelMap -> EnergyLevelMap
step1 = fmap (fmap (+1))


step2 :: EnergyLevelMap -> (EnergyLevelMap, [(Int, Int)])
step2 energyLevelMap =
    let step2Inner elm flashedOctos =
          let flashingOctopussies = List.filter (`notElem` flashedOctos) . List.filter (\ (x, y) -> elm!!x!!y > 9) $ coords
              newFlashedOctos = flashedOctos `union` flashingOctopussies
              levelUpOctos = List.filter (`notElem` flashedOctos) (flashingOctopussies >>= getValidNeighbourhood elm)
              newDeltaEnergyForOcto (x, y) = (List.length . List.filter (==(x, y)) $ levelUpOctos)
              elmIndexed = indexed elm
              newElm = List.map (\ (ix, xrow) -> List.map (\ (iy, value) -> newDeltaEnergyForOcto (ix, iy) + value) xrow) elmIndexed
          in if List.null flashingOctopussies
          then (elm, flashedOctos)
          else step2Inner newElm newFlashedOctos
    in step2Inner energyLevelMap []
    where coords = [(x, y) | x <- [0..(List.length energyLevelMap - 1)], y <- [0..((List.length . Prelude.head $ energyLevelMap) - 1)] ]
    
    
step3 :: ((EnergyLevelMap, [(Int, Int)]), Int) -> (EnergyLevelMap, Int)
step3 ((energyLevelMap, flashedOctos), flashCount) =
    let getNewValue x y value = if (x,y) `elem` flashedOctos then 0 else value
        energyLevelMapIndexed = indexed energyLevelMap
        newFlashCount = (+flashCount) . List.length $ flashedOctos
    in (List.map (\ (ix, xrow) -> List.map (uncurry (getNewValue ix)) xrow) energyLevelMapIndexed, newFlashCount)
    
    
step :: (EnergyLevelMap, Int) -> (EnergyLevelMap, Int)
step = step3 . Bifunctor.first (step2 . step1)

executeNSteps :: Int -> (EnergyLevelMap, Int) -> (EnergyLevelMap, Int)
executeNSteps n = List.foldr (.) id (Prelude.replicate n step)


checkSynchronization :: EnergyLevelMap -> Bool
checkSynchronization = List.all (List.all (==0))