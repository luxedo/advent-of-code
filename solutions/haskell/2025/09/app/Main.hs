{-  ElfScript Brigade
 -
 -  Advent Of Code 2025 Day 09
 -  Haskell Solution
 -
 -  Day 9: Movie Theater
 -
 -  https://adventofcode.com/2025/day/9 -}
{-# OPTIONS_GHC -Wno-x-partial #-}
module Main where

import           Data.Bifunctor
import           Data.Function
import           Data.List
import           Data.Maybe
import           Data.Ord
import           Fireplace

type Point = (Int, Int)

type RedTile = Point

type Area = Int

type TilePair = (RedTile, RedTile)

type Range = (Int, Int)

data Segment = Segment {x :: Int, yMin :: Int, yMax :: Int} deriving (Show, Eq)

data Slab = Slab {syMin :: Int, syMax :: Int, sxRanges :: [Range]} deriving (Show, Eq)

parse :: String -> [RedTile]
parse = map parseTile . lines

parseTile :: String -> RedTile
parseTile line = bimap read (read . drop 1) (break (== ',') line)

listAreas :: [RedTile] -> [(TilePair, Area)]
listAreas tiles = [((t1, t2), area t1 t2) | t1 <- tiles, t2 <- tiles]

listAreasSorted :: [RedTile] -> [(TilePair, Area)]
listAreasSorted = sortBy (comparing (Down . snd)) . listAreas

area :: RedTile -> RedTile -> Area
area (x1, y1) (x2, y2) = (abs (x2 - x1) + 1) * (abs (y2 - y1) + 1)

getVerticalSegments :: [Point] -> [Segment]
getVerticalSegments points =
  let pairs = zip points $ drop 1 $ cycle points
   in [Segment x1 (min y1 y2) (max y1 y2) | ((x1, y1), (x2, y2)) <- pairs, x1 == x2]

buildSlabs :: [Point] -> [Slab]
buildSlabs points =
  let segments = getVerticalSegments points
      ys = sort . nub $ concat [[y1, y2] | (y1, y2) <- map (\s -> (yMin s, yMax s)) segments]
      intervals = zip ys (drop 1 ys)
   in [Slab y1 y2 (findXRanges y1 y2 segments) | (y1, y2) <- intervals]
  where
    findXRanges y1 y2 segments =
      let activeX = sort [x s | s <- segments, yMin s <= y1 && yMax s >= y2]
       in toPairs activeX
    toPairs :: [Int] -> [Range]
    toPairs []             = []
    toPairs (a : b : rest) = (a, b) : toPairs rest
    toPairs [_]            = error "Missing pair"

inboundArea :: [Slab] -> RedTile -> RedTile -> Bool
inboundArea slabs (x1, y1) (x2, y2) =
  let (minX, maxX) = (min x1 x2, max x1 x2)
      (minY, maxY) = (min y1 y2, max y1 y2)
      overlappingSlabs = filter (\s -> syMax s > minY && syMin s < maxY) slabs
      totalYcovered = case overlappingSlabs of
        [] -> False
        ss -> minimum (map syMin ss) <= minY && maximum (map syMax ss) >= maxY
      isXCovered slab = any (\(px1, px2) -> px1 <= minX && px2 >= maxX) (sxRanges slab)
   in totalYcovered && all isXCovered overlappingSlabs

solvePt1 :: String -> [String] -> Int
solvePt1 input _args = do
  parse input & listAreasSorted & head & snd

solvePt2 :: String -> [String] -> Int
solvePt2 input _args = do
  let tiles = parse input
  let slabs = buildSlabs tiles
  listAreasSorted tiles & find (\((p0, p1), _) -> inboundArea slabs p0 p1) & fromJust & snd

main :: IO ()
main = do
  -- 🎅🎄❄️☃️🎁🦌
  -- Bright christmas lights HERE
  v1Run solvePt1 solvePt2
