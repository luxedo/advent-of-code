{-  ElfScript Brigade
 -
 -  Advent Of Code 2025 Day 10
 -  Haskell SolutionPt1
 -
 -  Day 10: Factory
 -
 -  https://adventofcode.com/2025/day/10 -}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Main where

import Control.Monad (join)
import Data.Bifunctor
import Data.Bits
import Data.Function
import Data.List
import Debug.Trace
import Fireplace

type Indicator = [Bool]

type Button = [Bool]

type Joltage = [Int]

type Machine = (Indicator, [Button], Joltage)

type SolutionPt1 = [Bool]
type SolutionPt2 = [Int]

parse :: String -> ([Indicator], [[Button]], [Joltage])
parse input =
  let machines = map parseMachine $ lines input
   in unzip3 machines

parseMachine :: String -> Machine
parseMachine line =
  let (lights, buttonsAndJoltage) = first (parseLights . tail) $ break (== ']') line
      (buttons, joltage) =
        bimap
          (parseButtons (length lights - 1) . tail . drop 1)
          (commaSplit . tail . init)
          (break (== '{') buttonsAndJoltage)
   in (lights, buttons, joltage)

parseLights :: String -> Indicator
parseLights = map parseLight
  where
    parseLight '.' = False
    parseLight '#' = True
    parseLight _ = error "Could not parse light"

parseButtons :: Int -> String -> [[Bool]]
parseButtons size =
  map (oneHot size . commaSplit . init . tail) . words

commaSplit :: String -> [Int]
commaSplit bs =
  let (before, after) = break (== ',') bs
   in ( (read before :: Int) : case after of
          [] -> []
          rest -> commaSplit (tail rest)
      )

oneHot :: Int -> [Int] -> [Bool]
oneHot size indexes = [i `elem` indexes | i <- [0 .. size]]

listPresses :: [Button] -> [SolutionPt1]
listPresses buttons = foldl filterGuesses (allGuesses $ length $ head buttons) buttons

listPressesJoltage :: [Joltage] -> [Button] -> [SolutionPt1]
listPressesJoltage joltage buttons = foldl filterGuesses (allGuesses $ length $ head buttons) buttons

allGuesses :: Int -> [[Bool]]
allGuesses 0 = [[]]
allGuesses n = [i : rest | i <- [True, False], rest <- allGuesses (n - 1)]

filterGuesses :: [SolutionPt1] -> Button -> [SolutionPt1]
filterGuesses guesses button = filter (isValid (last button) . zipWith (&&) (init button)) guesses

isValid :: Bool -> Button -> Bool
isValid light button = foldl xor False button == light

prettyMatrix :: (Show a) => [[a]] -> String
prettyMatrix = unlines . map (unwords . map show)

solvePt1 :: String -> [String] -> IO String
solvePt1 input _args = do
  let (indicator, buttons, _) = parse input
  let equations = zipWith (\i b -> transpose (b ++ [i])) indicator buttons
  pure $ show $ sum $ equations & map (minimum . map (sum . map fromEnum) . listPresses)

solvePt2 :: String -> [String] -> IO String
solvePt2 input _args = do
  let (indicator, buttons, joltage) = parse input
  let equations = zipWith (\i b -> transpose (b ++ [i])) indicator buttons
  pure $ show $ sum $ equations & map (minimum . map (sum . map fromEnum) . listPressesJoltage joltage)

main :: IO ()
main = do
  -- 🎅🎄❄️☃️🎁🦌
  -- Bright christmas lights HERE
  v1Run solvePt1 solvePt2

-- add[Button] :: Button -> Button -> Button
-- add[Button] = zipWith xor

-- gaussianEliminaion :: [Button] -> [Button]
-- gaussianEliminaion = anyTrue . nub . sorted . eliminate 0 0
--   where
--     sorted = sortBy (\a b -> compare (bitsToInt b) (bitsToInt a))
--     bitsToInt :: [Bool] -> Int
--     bitsToInt bs = foldl (\acc (i, b) -> if b then acc + 2 ^ (i :: Int) else acc) 0 (zip [0 ..] (reverse bs))
--     anyTrue = filter or
--
-- eliminate :: Int -> Int -> [Button] -> [Button]
-- eliminate r c buttons | r >= length buttons || c >= length (head buttons) = buttons
-- eliminate r c buttons =
--   let (before, targetAndAfter) = splitAt r buttons
--       (target : after) = targetAndAfter
--    in if target !! c then eliminate (r + 1) (c + 1) (join[Button] before target after) else eliminate r (c + 1) buttons
--   where
--     join[Button] before target after = map (clearRow target c) before ++ [target] ++ map (clearRow target c) after
--
-- clearRow :: Button -> Int -> Button -> Button
-- clearRow row c button = if button !! c then add[Button] row button else button

-- findFreeVariables :: [Button] -> ([Button], Guess)
-- findFreeVariables [] = ([], [])
-- findFreeVariables buttons@(x : xs) =
--   let (bs, answer) = (init x, last x)
--       (rest, gs) = findFreeVariables xs
--    in case sum $ map fromEnum bs of
--         1 ->
--           let guess = replaceAt (fromJust $ elemIndex True bs) (Just answer) initialGuess
--            in (rest, mergeGuess guess gs)
--         _ -> (x : rest, mergeGuess initialGuess gs)
--   where
--     initialGuess = [Nothing | _ <- [0 .. length (head buttons) - 2]]
--     replaceAt i newVal list = before ++ [newVal] ++ after
--       where
--         (before, _ : after) = splitAt i list
--     mergeGuess [] g = g
--     mergeGuess g [] = g
--     mergeGuess g1 g2 = zipWith mergeMaybes g1 g2
--
-- mergeMaybes :: (Eq a) => Maybe a -> Maybe a -> Maybe a
-- mergeMaybes (Just a) Nothing = Just a
-- mergeMaybes Nothing (Just a) = Just a
-- mergeMaybes Nothing Nothing = Nothing
-- mergeMaybes (Just a) (Just b) | a == b = Just a
-- mergeMaybes _ _ = error "Cannot merge Maybes"
--
-- toSolutionPt1 :: Guess -> SolutionPt1
-- toSolutionPt1 = map fromJust

-- fixGuess :: [Button] -> Guess -> ([Button], [Guess])
-- fixGuess buttons guess = ([], [])
