{-  ElfScript Brigade
 -
 -  Advent Of Code 2025 Day 05
 -  Haskell Solution
 -
 -  Day 5: Cafeteria
 -
 -  https://adventofcode.com/2025/day/5 -}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Main where

import Data.Function
import Data.Ix
import Data.List
import Fireplace

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
      | otherwise = split' u (tail s) (head s : subacc) acc

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
countIds = sum . map rangeSize . fixedPoint mergeRanges

mergeRanges :: [FreshRange] -> [FreshRange]
mergeRanges ranges = foldl mergeRange [] (sort ranges)

mergeRange :: [FreshRange] -> FreshRange -> [FreshRange]
mergeRange [] r = [r]
mergeRange (lastMerged : rest) newRange@(start, end) =
  let (lastStart, lastEnd) = lastMerged
   in if start <= lastEnd + 1
        then
          let mergedEnd = max lastEnd end
           in (lastStart, mergedEnd) : rest
        else
          newRange : lastMerged : rest

fixedPoint :: (Eq a) => (a -> a) -> a -> a
fixedPoint f x
  | x' == x = x
  | otherwise = fixedPoint f x'
  where
    x' = f x

solvePt1 :: String -> [String] -> IO String
solvePt1 input _args = do
  pure $ show $ parse input & uncurry countFresh

solvePt2 :: String -> [String] -> IO String
solvePt2 input _args = do
  pure $ show $ parse input & fst & countIds

main :: IO ()
main = do
  -- 🎅🎄❄️☃️🎁🦌
  -- Bright christmas lights HERE
  v1Run solvePt1 solvePt2
