module Aoc12
    ( solve12
    ) where

import System.IO
import Data.List as List
import Data.Text as Text
  

type Vertex = String
data Edge = Edge { v1 :: Vertex, v2 :: Vertex }
data Graph = Graph { vs :: [Vertex], es :: [Edge] }
type Path = [Vertex]

type CheckAugmentation = Path -> Vertex -> Bool


solve12 :: IO ()
solve12 = do
    handle <- openFile "assets/aoc12.txt" ReadMode
    content <- hGetContents handle
    let graph = parseGraph content
    print
      . List.length
      . generateAllPossibilities checkAugmentation1
      $ graph
    print
      . List.length
      . generateAllPossibilities checkAugmentation2
      $ graph
    hClose handle


checkAugmentation1 :: CheckAugmentation
checkAugmentation1 path v = isBigVertex v || v `notElem` path


checkAugmentation2 :: CheckAugmentation
checkAugmentation2 path v = 
    let smallCaveDoubleVisited = List.any (\ vert -> 2 <= (List.length . List.filter (== vert) $ path))
                               . List.filter (not . isBigVertex) 
                               $ path
        vCount = List.length . List.filter (==v) $ path
    in isBigVertex v || (v /= vertexInitial && not smallCaveDoubleVisited && vCount < 2) || vCount < 1


vertexInitial :: Vertex
vertexInitial = "start"


vertexTerminal :: Vertex
vertexTerminal = "end"
    
    
parseGraph :: String -> Graph
parseGraph inputString = 
    let inputLines = Prelude.lines inputString
        parseLine line = Text.unpack <$> Text.splitOn (Text.pack "-") (Text.pack line)
        vertices = List.concatMap parseLine inputLines
        edges = List.map ((\ [a, b] -> Edge { v1 = a, v2 = b }) . parseLine) inputLines
    in Graph { vs = vertices, es = edges }
    

isBigVertex :: Vertex -> Bool
isBigVertex v =
    (==vt) . Text.toUpper $ vt 
    where vt = Text.pack v
    

augmentationPossibilitiesPath :: CheckAugmentation -> Graph -> Path -> [Vertex]
augmentationPossibilitiesPath checkAugmentation Graph { vs = _, es = edges } (lastVertex : pathTail) =
    let path = (lastVertex : pathTail) 
        getOtherVertex Edge { v1 = a, v2 = b } = if a == lastVertex then b else a
        isAdjacentEdge Edge { v1 = a, v2 = b } = a == lastVertex || b == lastVertex
    in List.filter (checkAugmentation path)
     . List.map getOtherVertex 
     . List.filter isAdjacentEdge
     $ edges
     

pathBlackpill :: CheckAugmentation -> Graph -> Path -> Bool
pathBlackpill checkAugmentation graph (lastVertex : pathTail) =
    let path = lastVertex : pathTail
        augmentation = augmentationPossibilitiesPath checkAugmentation graph path
    in List.null augmentation && lastVertex /= vertexTerminal
    
    
augmentPath :: CheckAugmentation -> Graph -> Path -> [Path]
augmentPath checkAugmentation graph path =
    let augmentationPossibilities = augmentationPossibilitiesPath checkAugmentation graph path
    in if (== vertexTerminal) . List.head $ path 
    then [path]
    else List.map (: path) augmentationPossibilities
    
    
generateAllPossibilities :: CheckAugmentation -> Graph -> [Path]
generateAllPossibilities checkAugmentation graph =
    let augmentation :: [Path] -> Int -> [Path]
        augmentation paths _ = List.filter (not . pathBlackpill checkAugmentation graph) (paths >>= augmentPath checkAugmentation graph)
        infiniteAugmentation = List.scanl augmentation [[vertexInitial]] [0..]
        in fst
         . List.head
         . List.dropWhile (\ (a, b) -> List.length a /= List.length b) 
         . List.zip infiniteAugmentation
         $ List.tail infiniteAugmentation
