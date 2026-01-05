{-  ElfScript Brigade
 -
 -  Advent Of Code 2025 Day 04
 -  Haskell Solution
 -
 -  Day 4: Printing Department
 -
 -  https://adventofcode.com/2025/day/4 -}
module Main where

import           Data.Function
import           Data.Maybe
import           Fireplace

type Cafeteria = [[Slot]]

type Coordinates = ((Int, Int), Slot, Int)

data Slot = Empty | Paper deriving (Enum, Show, Eq)

parse :: String -> [[Slot]]
parse input = lines input & map (map parseSlot)

parseSlot :: Char -> Slot
parseSlot '@' = Paper
parseSlot '.' = Empty
parseSlot _   = error "Could not parse Slot"

enumerate :: [a] -> [(Int, a)]
enumerate = zip [0 :: Int ..]

countNeighbors :: Cafeteria -> [Coordinates]
countNeighbors c = index2d c & map (\((i, j), slot) -> ((i, j), slot, length $ slotsAround c i j & filter (== Paper)))

safeIndex :: Cafeteria -> Int -> Int -> Maybe Slot
safeIndex c@(c0 : _) i j
  | i < 0 = Nothing
  | j < 0 = Nothing
  | i >= length c = Nothing
  | j >= length c0 = Nothing
  | otherwise = Just ((c !! i) !! j)
safeIndex [] _ _ = error "Cannot index on empty"

slotsAround :: Cafeteria -> Int -> Int -> [Slot]
slotsAround c i j = [safeIndex c (i + dy) (j + dx) | dx <- [-1, 0, 1], dy <- [-1, 0, 1]] & catMaybes

index2d :: [[a]] -> [((Int, Int), a)]
index2d xss = [((row, col), val) | (row, xs) <- enumerate xss, (col, val) <- enumerate xs]

listRemovable :: Cafeteria -> [Coordinates]
listRemovable c = countNeighbors c & filter (\((_, _), slot, count) -> slot == Paper && count <= 4)

fixedPoint :: (Eq a) => (a -> a) -> a -> a
fixedPoint f x
  | x' == x = x
  | otherwise = fixedPoint f x'
  where
    x' = f x

removePapers :: Cafeteria -> Cafeteria
removePapers c = listRemovable c & applyRemoval c

removePaper :: Slot -> Slot
removePaper Paper = Empty
removePaper _     = error "Can only remove slot with Paper"

applyRemoval :: Cafeteria -> [Coordinates] -> Cafeteria
applyRemoval = foldl applyChange
  where
    applyChange c coord = update2d coord removePaper c

update2d :: Coordinates -> (a -> a) -> [[a]] -> [[a]]
update2d ((row, col), _, _) f grid =
  let (beforeRows, targetAndAfter) = splitAt row grid
   in case targetAndAfter of
        [] -> error "Cannot update1d on invalid col"
        targetRow : afterRows -> beforeRows ++ [update1d col f targetRow] ++ afterRows

update1d :: Int -> (a -> a) -> [a] -> [a]
update1d col f xs
  | col < 0 || col >= length xs = xs
  | otherwise =
      let (beforeCols, targetAndAfter) = splitAt col xs
       in case targetAndAfter of
            []                    -> error "Cannot update1d on invalid col"
            targetCol : afterCols -> beforeCols ++ [f targetCol] ++ afterCols

solvePt1 :: String -> [String] -> Int
solvePt1 input _args = do
  length $ parse input & listRemovable

solvePt2 :: String -> [String] -> Int
solvePt2 input _args = do
  let cafeteria = parse input
  length $ zipWith zip cafeteria (fixedPoint removePapers cafeteria) & concatMap (filter (== (Paper, Empty)))

main :: IO ()
main = do
  -- 🎅🎄❄️☃️🎁🦌
  -- Bright christmas lights HERE
  v1Run solvePt1 solvePt2
