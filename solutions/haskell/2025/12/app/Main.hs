{-  ElfScript Brigade
 -
 -  Advent Of Code 2025 Day 12
 -  Haskell Solution
 -
 -  Day 12: Christmas Tree Farm
 -
 -  https://adventofcode.com/2025/day/12 -}
{-# OPTIONS_GHC -Wno-x-partial #-}
module Main where

import           Data.Bifunctor
import           Data.Function
import           Data.List
import           Data.Maybe
import           Fireplace

type Shape = (Int, [[Bool]])

data Region = Region {width :: Int, height :: Int, shapeIdx :: [Int]} deriving (Show)

parse :: String -> ([Shape], [Region])
parse = bimap (map parseShape) (map parseRegion . lines) . fromJust . unsnoc . splitStr "\n\n"

parseShape :: String -> Shape
parseShape s =
  let (idx, rest) = bimap read (drop 2) (break (== ':') s)
      shape = map (map parseCell) $ lines rest
   in (idx, shape)
  where
    parseCell '.' = False
    parseCell '#' = True
    parseCell _   = error "Cannot parse cell"

parseRegion :: String -> Region
parseRegion r =
  let (width, heightAndRest) = break (== 'x') r & first read
      (height, rest) = break (== ':') (drop 1 heightAndRest) & first read
      shapeIdx = map read $ words (drop 1 rest)
   in Region width height shapeIdx

splitStr :: (Eq a) => [a] -> [a] -> [[a]]
splitStr _ [] = []
splitStr sub str = split' sub str [] []
  where
    split' _ [] subacc acc = reverse (reverse subacc : acc)
    split' u s subacc acc
      | u `isPrefixOf` s = split' u (drop (length u) s) [] (reverse subacc : acc)
      | otherwise = split' u (drop 1 s) (head s : subacc) acc

packingFits :: [Shape] -> [Region] -> [Region]
packingFits shapes regions =
  -- TODO: proper solution with backtracking
  let firstShape = snd $ head shapes
      (height, width) = (length firstShape, length $ head firstShape)
   in filter (fitArea height width) regions
  where
    fitArea h w region =
      let fitX = width region `div` w
          fitY = height region `div` h
       in fitX * fitY >= sum (shapeIdx region)

solvePt1 :: String -> [String] -> IO String
solvePt1 input _args = do
  pure $ show $ length $ parse input & uncurry packingFits

solvePt2 :: String -> [String] -> IO String
solvePt2 _input _args = do
  pure "December"

main :: IO ()
main = do
  -- 🎅🎄❄️☃️🎁🦌
  -- Bright christmas lights HERE
  v1Run solvePt1 solvePt2
