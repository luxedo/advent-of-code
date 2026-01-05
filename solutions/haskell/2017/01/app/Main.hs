{-  ElfScript Brigade
 -
 -  Advent Of Code 2017 Day 01
 -  Haskell Solution
 -
 -  Day 1: Inverse Captcha
 -
 -  https://adventofcode.com/2017/day/1 -}
module Main where

import           Data.Char (digitToInt, isSpace)
import           Fireplace

solvePt1 :: String -> [String] -> Int
solvePt1 input _args = do
  let captcha = filter (not . isSpace) input
  sum $ map (digitToInt . fst) $ filter (uncurry (==)) $ zip captcha (drop 1 $ cycle captcha)

solvePt2 :: String -> [String] -> String
solvePt2 _input _args = do
  "December"

main :: IO ()
main = do
  -- 🎅🎄❄️☃️🎁🦌
  -- Bright christmas lights HERE
  v1Run solvePt1 solvePt2
