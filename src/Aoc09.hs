module Aoc09
    ( solve09
    ) where

import System.IO
import Data.Char as Char
import Data.List as List hiding ((\\))
import Data.List.Split
import Data.Maybe as Maybe
import Data.Set as Set
import Data.Text

import Debug.Trace
  
  
type Heightmap = [[Int]]
type Basin = Set (Int, Int)


solve09 :: IO ()
solve09 = do
    handle <- openFile "assets/aoc09.txt" ReadMode
    content <- hGetContents handle
    let heightmap = parseMatrix content
    print . sumOfLowPoints $ heightmap
    print (basinification heightmap . getAllLowPoints $ heightmap)
    hClose handle
    
parseMatrix :: String -> Heightmap
parseMatrix = fmap (fmap (\ c -> read [c])) . Prelude.lines


tryGetValue :: Heightmap -> Int -> Int -> Maybe Int
tryGetValue heightmap x y =
    let xValid = 0 <= x && x < List.length heightmap  
        yValid = 0 <= y && y < List.length (heightmap!!x)  
    in if xValid && yValid 
    then Just (heightmap!!x!!y)
    else Nothing
    
    
determineNeighbourhood :: Heightmap -> Int -> Int -> [Int]
determineNeighbourhood heightmap x y =
    Maybe.mapMaybe (uncurry (tryGetValue heightmap)) coords
    where coords = [(x + dx, y + dy) | (dx, dy) <- [(0,1), (1,0), (-1,0), (0,-1)]]
    
    
getAllLowPoints :: Heightmap -> [(Int, Int)]
getAllLowPoints heightmap =
    let isLowPoint (x, y) = List.all (\ v -> heightmap!!x!!y < v) $ determineNeighbourhood heightmap x y
        xrange = [0..(List.length heightmap - 1)]
        yrange = [0..(List.length (Prelude.head heightmap) - 1)]
    in List.filter isLowPoint [(x, y) | x <- xrange, y <- yrange]
    
    
sumOfLowPoints :: Heightmap -> Int
sumOfLowPoints heightmap =
    List.sum
    . List.map (\ (x, y) -> 1 + heightmap!!x!!y) 
    . getAllLowPoints
    $ heightmap
    
    
determineNeighbourhoodValidPoints :: Heightmap -> (Int, Int) -> [(Int, Int)]
determineNeighbourhoodValidPoints heightmap (x, y) = 
    [(x + dx, y + dy) | 
      (dx, dy) <- [(0,1), (1,0), (-1,0), (0,-1)], 
      0 <= x + dx && x + dx < List.length heightmap, 
      0 <= y + dy && y + dy < List.length (heightmap!!(x + dx)) ]
    
    
  
lowPointToBasin :: Heightmap -> (Int, Int) -> Basin
lowPointToBasin heightmap (x, y) =
    let lowPointToBasinInner :: Set (Int, Int) -> Basin
        lowPointToBasinInner protoBasin =
            let neighbourhoodPlus :: Set (Int, Int)
                neighbourhoodPlus = 
                    Set.unions 
                  . Set.map (Set.fromList . determineNeighbourhoodValidPoints heightmap) 
                  $ protoBasin
                neighbourhood = neighbourhoodPlus \\ protoBasin
                basinExtension = Set.filter (\ (xi, yi) -> heightmap!!xi!!yi < 9) neighbourhood
            in if Set.null basinExtension
            then protoBasin
            else lowPointToBasinInner . Set.union protoBasin $ basinExtension
    in lowPointToBasinInner (Set.singleton (x, y))
    
    
basinification :: Heightmap -> [(Int, Int)] -> Int
basinification heightmap lowPoints =
    let basins = lowPointToBasin heightmap <$> lowPoints
        biggestBasins = List.take 3 . List.sortOn (0-) . List.map Set.size $ basins
    in product biggestBasins