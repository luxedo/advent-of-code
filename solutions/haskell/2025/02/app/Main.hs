{-  ElfScript Brigade
 -
 -  Advent Of Code 2025 Day 02
 -  Haskell Solution
 -
 -  Day 2: Gift Shop
 -
 -  https://adventofcode.com/2025/day/2 -}
{-# OPTIONS_GHC -Wno-x-partial #-}
module Main where

import           Data.Function
import           Data.List
import           Fireplace

newtype IdRange = IdRange (Id, Id) deriving (Show)

type Id = Int

split :: (Eq a) => a -> [a] -> [[a]]
split _ [] = [[]]
split c st = pf : rst
  where
    rst
      | (_ : sf1) <- sf = split c sf1
      | otherwise = []
    (pf, sf) = span (c /=) st

parse :: String -> [IdRange]
parse input = splitStr "," input & map parseIdRange

splitStr :: (Eq a) => [a] -> [a] -> [[a]]
splitStr _ [] = []
splitStr sub str = split' sub str [] []
  where
    split' _ [] subacc acc = reverse (reverse subacc : acc)
    split' u s subacc acc
      | u `isPrefixOf` s = split' u (drop (length u) s) [] (reverse subacc : acc)
      | otherwise = split' u (drop 1 s) (head s : subacc) acc



parseIdRange :: String -> IdRange
parseIdRange s = mkIdRange (split '-' s)

mkIdRange :: [String] -> IdRange
mkIdRange [low, high] = IdRange (read low :: Id, read high :: Id)
mkIdRange _           = error (show "Cannot parse IdRange")

splitHalf :: Id -> [Id]
splitHalf x = [x `mod` d, x `div` d]
  where
    d = 10 ^ (length (show x) `div` 2)

chunks :: Int -> [a] -> [[a]]
chunks _ [] = []
chunks n xs
  | length xs `div` n /= 0 = []
  | otherwise = take n xs : chunks n (drop n xs)

splitInt :: Int -> Int -> [Int]
splitInt n x = chunks n (show x) & map read

invalidIdsPt1 :: Id -> Id -> [Id]
invalidIdsPt1 low high = do
  map (\x -> x * 10 ^ length (show x) + x) [l .. h]
  where
    l = minimum (splitHalf low)
    h = maximum (splitHalf high)

possibleSlices :: Int -> [(Int, Int)]
possibleSlices s = [1 .. l `div` 2] & map (\x -> (x, l `div` x)) & filter (\(x, m) -> x * m == l)
  where
    l = length (show s)

buildInvalidIds :: (Int, Int) -> [Id]
buildInvalidIds (i, n) = [10 ^ (i - 1) :: Int .. (10 ^ i) - 1 :: Int] & map (read . concat . replicate n . show)

invalidIdsPt2 :: Id -> Id -> [Id]
invalidIdsPt2 low high = nub (possibleSlices low ++ possibleSlices high) & concatMap buildInvalidIds

searchInvalid :: (Id -> Id -> [Id]) -> IdRange -> [Id]
searchInvalid fn (IdRange (low, high)) = fn low high & filter (\x -> x >= low && x <= high)

solvePt1 :: String -> [String] -> IO String
solvePt1 input _args = do
  pure (parse input & concatMap (searchInvalid invalidIdsPt1) & sum & show)

solvePt2 :: String -> [String] -> IO String
solvePt2 input _args = do
  pure (parse input & concatMap (searchInvalid invalidIdsPt2) & nub & sum & show)

main :: IO ()
main = do
  -- 🎅🎄❄️☃️🎁🦌
  -- Bright christmas lights HERE
  v1Run solvePt1 solvePt2
