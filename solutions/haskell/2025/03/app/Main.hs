{-  ElfScript Brigade
 -
 -  Advent Of Code 2025 Day 03
 -  Haskell Solution
 -
 -  Day 3: Lobby
 -
 -  https://adventofcode.com/2025/day/3 -}
module Main where

import Data.Function
import Fireplace

type Bank = [Battery]

type Battery = Int

type Joltage = Int

parse :: String -> [Bank]
parse input = map parseBank (split '\n' input)

parseBank :: String -> [Battery]
parseBank = map parseBattery

parseBattery :: Char -> Battery
parseBattery b = read [b] :: Battery

split :: (Eq a) => a -> [a] -> [[a]]
split _ [] = []
split c st = pf : rst
  where
    rst
      | (_ : sf1) <- sf = split c sf1
      | otherwise = []
    (pf, sf) = span (c /=) st

argmax :: (Ord a) => [a] -> (a, Int)
argmax xs = foldr (\(x, y) acc -> if x == maximum xs then (x, y) else acc) (head xs, 0) (zip xs [0 ..])

largestJoltage :: Int -> Bank -> Joltage
largestJoltage 1 bank = maximum bank
largestJoltage n bank = do
  let (m, i) = argmax $ take (length bank - n + 1) bank
  (m * 10 ^ (n - 1)) + largestJoltage (n - 1) (drop (i + 1) bank)

solvePt1 :: String -> [String] -> IO String
solvePt1 input _args = do
  pure (show $ sum $ parse input & map (largestJoltage 2))

solvePt2 :: String -> [String] -> IO String
solvePt2 input _args = do
  pure (show $ sum $ parse input & map (largestJoltage 12))

main :: IO ()
main = do
  -- 🎅🎄❄️☃️🎁🦌
  -- Bright christmas lights HERE
  v1Run solvePt1 solvePt2
