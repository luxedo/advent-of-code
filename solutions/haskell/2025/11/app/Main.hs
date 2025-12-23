{-  ElfScript Brigade
 -
 -  Advent Of Code 2025 Day 11
 -  Haskell Solution
 -
 -  Day 11: Reactor
 -
 -  https://adventofcode.com/2025/day/11 -}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Main where

import Control.Arrow
import Data.Function
import Data.Map qualified as M
import Data.Map qualified as Map
import Data.Maybe (fromMaybe)
import Fireplace

type Rack = [Server]

type Name = String

type Cables = [Name]

type Server = (Name, Cables)

type Path = [Name]

type RackMap = M.Map Name Cables

parse :: String -> RackMap
parse = Map.insert "out" [] . Map.fromList . map parseServer . lines

parseServer :: String -> Server
parseServer line = break (== ':') line & second (words . tail)

countPaths :: Name -> Name -> RackMap -> Int
countPaths start end cache = counts M.! start
  where
    counts = M.fromSet calculate (M.keysSet cache)
    calculate node
      | node == end = 1
      | otherwise = sum [fromMaybe 0 (M.lookup child counts) | child <- fromMaybe [] (M.lookup node cache)]

solvePt1 :: String -> [String] -> IO String
solvePt1 input _args =
  pure $ show $ parse input & countPaths "you" "out"

solvePt2 :: String -> [String] -> IO String
solvePt2 input _args = do
  pure $ show $ parse input & (go "fft" "dac" &&& go "dac" "fft") & uncurry (+)
  where
    go node1 node2 graph =
      product
        [ countPaths "svr" node1 graph,
          countPaths node1 node2 graph,
          countPaths node2 "out" graph
        ]

main :: IO ()
main = do
  -- 🎅🎄❄️☃️🎁🦌
  -- Bright christmas lights HERE
  v1Run solvePt1 solvePt2
