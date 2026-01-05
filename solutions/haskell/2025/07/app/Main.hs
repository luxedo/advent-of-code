{-  ElfScript Brigade
 -
 -  Advent Of Code 2025 Day 07
 -  Haskell Solution
 -
 -  Day 7: Laboratories
 -
 -  https://adventofcode.com/2025/day/7 -}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Main where

import           Data.Function
import           Fireplace

type TachyonManifold = [[Slot]]

data Slot = Beam Int | Splitter Int deriving (Eq)

parse :: String -> TachyonManifold
parse input = lines input & map (map parseSlot)

parseSlot :: Char -> Slot
parseSlot 'S' = Beam 1
parseSlot '.' = Beam 0
parseSlot '^' = Splitter 0
parseSlot _   = error "Could not parse Slot"

runBeam :: Int -> TachyonManifold -> TachyonManifold
runBeam height manifold
  | height >= length manifold - 1 = manifold
runBeam height manifold =
  let (before, targetAndAfter) = splitAt height manifold
      (target : next : after) = targetAndAfter
      newNext = updateRow target next
   in runBeam (height + 1) (before ++ [target, newNext] ++ after)

updateRow :: [Slot] -> [Slot] -> [Slot]
updateRow target next =
  let initialBeams = replicate (length next) 0
      newBeams = foldr iterBeam initialBeams (zip [0 ..] target)
   in zipWith setBeam next newBeams
  where
    iterBeam :: (Int, Slot) -> [Int] -> [Int]
    iterBeam (col, Beam count) = addAt col count
    iterBeam (col, Splitter count) = addAt (col - 1) count . addAt (col + 1) count
    setBeam :: Slot -> Int -> Slot
    setBeam (Beam _)     = Beam
    setBeam (Splitter _) = Splitter
    addAt :: (Num a) => Int -> a -> [a] -> [a]
    addAt i newVal list = take i list ++ [(list !! i) + newVal] ++ drop (i + 1) list

countBeams :: [Slot] -> [Int]
countBeams = map countBeam
  where
    countBeam s = case s of
      Splitter i -> i
      Beam i     -> i

splitersHit :: [Slot] -> Int
splitersHit = length . filter splitterHit
  where
    splitterHit s = case s of
      Splitter i | i > 0 -> True
      _                  -> False

prettyPrintMatrix :: (Show a) => [[a]] -> IO ()
prettyPrintMatrix = mapM_ (putStrLn . unwords . map show)

solvePt1 :: String -> [String] -> Int
solvePt1 input _args = do
  parse input & runBeam 0 & map splitersHit & sum

solvePt2 :: String -> [String] -> Int
solvePt2 input _args = do
  parse input & runBeam 0 & last & countBeams & sum

main :: IO ()
main = do
  -- 🎅🎄❄️☃️🎁🦌
  -- Bright christmas lights HERE
  v1Run solvePt1 solvePt2
