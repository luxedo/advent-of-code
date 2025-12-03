{-  ElfScript Brigade
 -
 -  Advent Of Code 2025 Day 01
 -  Haskell Solution
 -
 -  Day 1: Secret Entrance
 -
 -  https://adventofcode.com/2025/day/1 -}
{-# LANGUAGE LambdaCase #-}

module Main where

import Data.Function
import Data.Maybe qualified
import Fireplace

data Side = LeftRot | RightRot deriving (Enum, Show)

parseSide :: Char -> Maybe Side
parseSide 'L' = Just LeftRot
parseSide 'R' = Just RightRot
parseSide _ = Nothing

newtype Rotate = Rotate (Int, Side) deriving (Show)

parseRotate :: String -> Maybe Rotate
parseRotate [] = Nothing
parseRotate s = do
  let (c : cs) = s
  let num = read cs :: Int
  case parseSide c of
    Just side -> Just (Rotate (num, side))
    Nothing -> Nothing

collect :: [Maybe a] -> [a]
collect = Data.Maybe.fromMaybe (error "Could not collect") . sequence

parse :: String -> [Rotate]
parse input = words input & map parseRotate & collect

newtype Dial = Dial (Int, Int) deriving (Show, Eq)

rotate :: Dial -> Int -> Dial
rotate (Dial (p, w)) x = do
  let r = p + x
  let (q, newPos) = divMod r 100
  let f1 = (if x <= 0 && p == 0 then -1 else 0) -- fix going left from zero
  let f2 = (if x <= 0 && newPos == 0 then 1 else 0) -- fix going left to zero
  let wrap = abs q + w + f1 + f2
  Dial (newPos, wrap)

rotateDial :: Dial -> Rotate -> Dial
rotateDial d (Rotate (x, LeftRot)) = rotate d (-x)
rotateDial d (Rotate (x, RightRot)) = rotate d x

rotateMany :: Dial -> [Rotate] -> [Dial]
rotateMany = scanl rotateDial

countZeros :: [Dial] -> Int
countZeros = length . filter (\case Dial (0, _) -> True; _ -> False)

countZeroPasses :: Dial -> Int
countZeroPasses (Dial (_, w)) = w

solvePt1 :: String -> [String] -> IO String
solvePt1 input _args = do
  let rotations = parse input
  pure (show (Dial (50, 0) `rotateMany` rotations & countZeros))

solvePt2 :: String -> [String] -> IO String
solvePt2 input _args = do
  let rotations = parse input
  pure (show (Dial (50, 0) `rotateMany` rotations & last & countZeroPasses))

main :: IO ()
main = do
  -- 🎅🎄❄️☃️🎁🦌
  -- Bright christmas lights HERE
  v1Run solvePt1 solvePt2
