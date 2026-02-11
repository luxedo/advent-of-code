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

import           Data.Bifunctor
import           Data.Bits
import           Data.Function
import           Data.List
import           Fireplace

type Indicator = [Bool]

type Button = [Bool]

type Joltage = [Int]

type Machines = [Machine]

data Machine = Machine {buttons :: Buttons, indicator :: Indicator, joltage :: Joltage}

data Buttons = Buttons {bool :: [Button], int :: [[Int]]} deriving (Show)

parse :: String -> [Machine]
parse input = map parseMachine $ lines input

parseMachine :: String -> Machine
parseMachine line =
  let (indicator, buttonsAndJoltage) = first (parseLights . drop 1) $ break (== ']') line
      (buttons, joltage) =
        bimap
          (parseButtons (length indicator - 1) . drop 1 . drop 1)
          (commaSplit . drop 1 . init)
          (break (== '{') buttonsAndJoltage)
   in Machine{buttons = buttons, indicator = indicator, joltage = joltage}

parseLights :: String -> Indicator
parseLights = map parseLight
 where
  parseLight '.' = False
  parseLight '#' = True
  parseLight _   = error "Could not parse light"

parseButtons :: Int -> String -> Buttons
parseButtons size input =
  let
    buttonsInt = map (commaSplit . init . drop 1) . words $ input
    buttonsBool = map (oneHot size) buttonsInt
   in
    Buttons{bool = buttonsBool, int = buttonsInt}

commaSplit :: String -> [Int]
commaSplit bs =
  let (before, after) = break (== ',') bs
   in (read before :: Int) : case after of
        []   -> []
        rest -> commaSplit (drop 1 rest)

oneHot :: Int -> [Int] -> [Bool]
oneHot size indexes = [i `elem` indexes | i <- [0 .. size]]

listPresses :: [Button] -> [[Bool]]
listPresses [] = []
listPresses buttons@(b : _) = foldl filterGuesses (allGuesses $ length b) buttons

allGuessesCache :: [[[Bool]]]
allGuessesCache = map generate [0 ..]
 where
  generate 0 = [[]]
  generate n = [i : rest | i <- [True, False], rest <- allGuessesCache !! (n - 1)]
allGuesses :: Int -> [[Bool]]
allGuesses n = allGuessesCache !! n

filterGuesses :: [[Bool]] -> Button -> [[Bool]]
filterGuesses guesses button = filter (isValid (last button) . zipWith (&&) (init button)) guesses

isValid :: Bool -> Button -> Bool
isValid light button = foldl xor False button == light

machineToEq :: Machine -> (Int, [(Int, Int)])
machineToEq Machine{buttons = Buttons{int}, joltage} = (2 ^ length int - 1, eqs)
 where
  eqs = augment [(bitmaskForIndex i, target) | (i, target) <- zip [0 ..] joltage]
  btns = map (foldr ((\b acc -> acc + (2 :: Int) ^ b) . fromEnum) 0) int
  pairs = zip [0 :: Int ..] btns
  bitmaskForIndex i = sum $ [if b .&. (1 `shiftL` i) > 0 then 2 ^ j else 0 | (j, b) <- pairs]

augment :: [(Int, Int)] -> [(Int, Int)]
augment eqs = eqs ++ [(vb - va, tb - ta) | (va, ta) <- eqs, (vb, tb) <- eqs, tb > ta && va .&. vb == va]

minUsing :: (Ord b) => (a -> b) -> [a] -> a
minUsing fn = minimumBy (compare `on` fn)

toBits :: Int -> [Int]
toBits 0 = []
toBits n = go 0 n
 where
  go _ 0 = []
  go i x = if x .&. (1 `shiftL` i) > 0 then i : go (i + 1) (x - (1 `shiftL` i)) else go (i + 1) x

-- all the ways that N numbers can sum to S
waysToSum :: Int -> Int -> [[Int]] -- S -> N -> list
waysToSum 0 0 = [[]]
waysToSum 0 n = [replicate n 0]
waysToSum _ 0 = []
waysToSum s n = concat [map (a :) $ waysToSum (s - a) (n - 1) | a <- [0 .. s]]

substitute :: [(Int, Int)] -> (Int, Int) -> (Int, Int)
substitute [] vt = vt
substitute ((var, val) : rest) vt@(vars, target)
  | vars .&. (1 `shiftL` var) > 0 = substitute rest (vars - (1 `shiftL` var), target - val)
  | otherwise = substitute rest vt

isValidEq :: (Int, Int) -> Bool
isValidEq (0, 0) = True
isValidEq (0, _) = False
isValidEq (_, v) = v >= 0

minimizeSystem :: Int -> [(Int, Int)] -> Int
minimizeSystem 0 _ = 0
minimizeSystem remainingBits eqs = target + bestOfRest
 where
  chosenEq@(eq, target) = minUsing (popCount . fst) eqs
  bits = toBits eq
  candidates = map (zip bits) $ waysToSum target (length bits)
  otherEqs = filter (chosenEq /=) eqs
  filteredCandidates =
    [ (remainingBits - eq, remainingEqs)
    | candidate <- candidates
    , remainingEqs <- [map (substitute candidate) otherEqs]
    , all isValidEq remainingEqs
    ]
  bestOfRest =
    if null filteredCandidates
      then 100000
      else
        minimum $ map (uncurry minimizeSystem) filteredCandidates

solvePt1 :: String -> [String] -> Int
solvePt1 input _args = do
  let machines = parse input
  let buttonMatrix = map (bool . buttons) machines
  let indicators = map indicator machines
  let equations = zipWith (\i b -> transpose (b ++ [i])) indicators buttonMatrix
  sum $ map (minimum . map (sum . map fromEnum) . listPresses) equations

-- Comment of shame. I coundn't do it. I'll try again eventually 👀. I don't wanna do this anymore
-- https://www.youtube.com/watch?v=GROredXDutI
-- https://github.com/danvk/aoc2025/blob/main/y2025d10/Main.hs
solvePt2 :: String -> [String] -> Int
solvePt2 input _args = sum $ map (uncurry minimizeSystem . machineToEq) (parse input)

main :: IO ()
main = do
  -- 🎅🎄❄️☃️🎁🦌
  -- Bright christmas lights HERE
  v1Run solvePt1 solvePt2
