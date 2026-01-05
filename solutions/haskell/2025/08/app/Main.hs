{-  ElfScript Brigade
 -
 -  Advent Of Code 2025 Day 08
 -  Haskell Solution
 -
 -  Day 8: Playground
 -
 -  https://adventofcode.com/2025/day/8 -}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns -Wno-x-partial #-}

module Main where

import           Data.Function
import           Data.List
import           Data.Maybe
import           Data.Ord
import           Fireplace
import           GHC.Float

parseArgs :: [String] -> Int
parseArgs [s] = read s :: Int
parseArgs []  = 1000
parseArgs _   = error "Incorrect args"

type Box = Vector

type Vector = (Int, Int, Int)

type Boxes = [Box]

type Connection = ((Int, Int), Double)

newtype Circuit = Circuit (Int, [Int]) deriving (Show, Eq)

type Group = [Circuit]

type Nodes = [Int]

parse :: String -> [Box]
parse = map parseBox . lines

parseBox :: String -> Box
parseBox line =
  let (x, rest) = break (== ',') line
      (y, z) = break (== ',') (drop 1 rest)
   in (read x, read y, read $ drop 1 z)

distances :: [Box] -> [Connection]
distances [] = []
distances (box : rest) = zipWith mkDistance [1 ..] rest ++ (distances rest & map incrementIndex)
  where
    mkDistance i r = ((0, i), normalize $ distance box r)
    incrementIndex ((i, j), d) = ((i + 1, j + 1), d)

distance :: Box -> Box -> Vector
distance (x1, y1, z1) (x2, y2, z2) = (x2 - x1, y2 - y1, z2 - z1)

normalize :: Vector -> Double
normalize (x, y, z) = sqrt $ int2Double $ x ^ (2 :: Int) + y ^ (2 :: Int) + z ^ (2 :: Int)

-- normalize (x, y, z) = int2Double $ x ^ (2 :: Int) + y ^ (2 :: Int) + z ^ (2 :: Int)
-- normalize (x, y, z) = int2Double $ x + y + z

connect :: Connection -> [Circuit] -> [Circuit]
connect connection circuits =
  let circuitForward = connectCircuit (fst connection) circuits
   in connectCircuit (rev $ fst connection) circuitForward
  where
    rev (a, b) = (b, a)

connectCircuit :: (Int, Int) -> [Circuit] -> [Circuit]
connectCircuit (src, dst) circuits =
  let (before, targetAndAfter) = circuits & splitAt src
      (target : after) = targetAndAfter
   in before ++ [prepend dst target] ++ after
  where
    prepend x (Circuit (i, xs)) = Circuit (i, x : xs)

hasConnections :: Circuit -> Bool
hasConnections (Circuit (_, conn)) = not $ null conn

findGroups :: [Group] -> [Circuit] -> [Group]
findGroups g [] = g
findGroups g circuits =
  let found = dfs circuits [] [head circuits]
      reduced = [c | c <- circuits, c `notElem` found]
   in findGroups (found : g) reduced

dfs :: [Circuit] -> [Circuit] -> [Circuit] -> [Circuit]
dfs _ visited [] = reverse visited
dfs graph visited (x : xs)
  | x `elem` visited = dfs graph visited xs
  | otherwise = dfs graph (x : visited) (getCircuits x ++ xs)
  where
    getCircuits (Circuit (_, cs)) = map getCircuit cs
    getCircuit i = fromJust $ find (matchIndex i) graph
    matchIndex i (Circuit (j, _)) = i == j

getGroupNodes :: [Group] -> [Nodes]
getGroupNodes [] = []
getGroupNodes (x : xs) = map listNodes x : getGroupNodes xs
  where
    listNodes (Circuit (i, _)) = i

connectFull :: [Circuit] -> [Connection] -> [Connection] -> ([Circuit], [Connection])
connectFull circuits connected [] = (circuits, connected)
connectFull circuits connected (conn : conns) =
  let newCircuit = connect conn circuits
   in if all hasConnections newCircuit && length (findGroups [] newCircuit) == 1
        then
          (newCircuit, conn : connected)
        else connectFull newCircuit (conn : connected) conns

solvePt1 :: String -> [String] -> Int
solvePt1 input args = do
  let pairs = parseArgs args
  let boxes = parse input
  let circuits = [Circuit (i, []) | i <- [0 .. length boxes - 1]]
  let connections = boxes & distances & sortBy (comparing snd) & take pairs
  let connected = foldr connect circuits connections & filter hasConnections & findGroups []
  sortBy (comparing Data.Ord.Down) (connected & getGroupNodes & map length) & take 3 & product

solvePt2 :: String -> [String] -> Int
solvePt2 input _args = do
  let boxes = parse input
  let circuits = [Circuit (i, []) | i <- [0 .. length boxes - 1]]
  let connections = boxes & distances & sortBy (comparing snd)
  let finalConnections = connectFull circuits [] connections & snd
  let (c1, c2) = head finalConnections & fst
  getX (boxes !! c1) * getX (boxes !! c2)
  where
    getX (x, _, _) = x


main :: IO ()
main = do
  -- 🎅🎄❄️☃️🎁🦌
  -- Bright christmas lights HERE
  v1Run solvePt1 solvePt2
