{-  ElfScript Brigade
 -
 -  Advent Of Code 2025 Day 05
 -  Haskell Solution
 -
 -  Day 5: Cafeteria
 -
 -  https://adventofcode.com/2025/day/5 -}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns -Wno-x-partial #-}
module Main where

import           Data.Function
import           Data.Ix
import           Data.List
import           Fireplace

type Ingredient = Int

type FreshRange = (Int, Int)

parse :: String -> ([FreshRange], [Ingredient])
parse input =
  let [rs, is] = splitStr "\n\n" input
   in (parseRanges rs, parseIngredients is)

splitStr :: (Eq a) => [a] -> [a] -> [[a]]
splitStr _ [] = []
splitStr sub str = split' sub str [] []
  where
    split' _ [] subacc acc = reverse (reverse subacc : acc)
    split' u s subacc acc
      | u `isPrefixOf` s = split' u (drop (length u) s) [] (reverse subacc : acc)
      | otherwise = split' u (drop 1 s) (head s : subacc) acc

parseRanges :: String -> [FreshRange]
parseRanges = map parseRange . lines

parseRange :: String -> FreshRange
parseRange r =
  let [start, end] = splitStr "-" r & map read
   in (start, end)

parseIngredients :: String -> [Ingredient]
parseIngredients = map read . lines

countFresh :: [FreshRange] -> [Ingredient] -> Int
countFresh ranges = length . filter anyRange
  where
    anyRange i = any (`inRange` i) ranges

countIds :: [FreshRange] -> Int
countIds = sum . map rangeSize . mergeRanges . sort

mergeRanges :: [FreshRange] -> [FreshRange]
mergeRanges [] = []
mergeRanges [r] = [r]
mergeRanges ((s1, e1) : (s2, e2) : rest)
  | e1 >= s2 - 1 = mergeRanges ((min s1 s2, max e1 e2) : rest)
mergeRanges (r : rest) = r : mergeRanges rest

solvePt1 :: String -> [String] -> Int
solvePt1 input _args = do
  parse input & uncurry countFresh

solvePt2 :: String -> [String] -> Int
solvePt2 input _args = do
  parse input & fst & countIds

main :: IO ()
main = do
  -- 🎅🎄❄️☃️🎁🦌
  -- Bright christmas lights HERE
  v1Run solvePt1 solvePt2
