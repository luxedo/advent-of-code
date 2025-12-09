{-  ElfScript Brigade
 -
 -  Advent Of Code 2025 Day 06
 -  Haskell Solution
 -
 -  Day 6: Trash Compactor
 -
 -  https://adventofcode.com/2025/day/6 -}

module Main where

import Data.Char
import Data.Function
import Data.List
import Fireplace

data Operator = Add | Mul deriving (Show)

data Equation = Equation
  { operator :: Operator,
    operands :: [Int]
  }
  deriving (Show)

type Dialect = ([String] -> [[String]])

getOperation :: Operator -> ([Int] -> Int)
getOperation Add = sum
getOperation Mul = product

parse :: String -> Dialect -> [Equation]
parse input dialect =
  let allLines = lines input
      operands = map parseOperands $ dialect $ init allLines
      operators = map parseOperator $ words $ last allLines
   in zipWith Equation operators operands

regularNotation :: Dialect
regularNotation = transpose . map words

isSpaceBlock :: String -> Bool
isSpaceBlock = all isSpace

cephalopodNotation :: Dialect
cephalopodNotation =
  filter (not . null)
    . map (filter (not . isSpaceBlock))
    . groupBy (\a b -> isSpaceBlock a == isSpaceBlock b)
    . transpose

parseOperands :: [String] -> [Int]
parseOperands = map read

parseOperator :: String -> Operator
parseOperator "+" = Add
parseOperator "*" = Mul
parseOperator _ = error "Could not parse Operator"

solveEquation :: Equation -> Int
solveEquation Equation {operator, operands} = getOperation operator operands

solveEquations :: String -> Dialect -> String
solveEquations input notation = show $ parse input notation & map solveEquation & sum

solvePt1 :: String -> [String] -> IO String
solvePt1 input _args = do
  pure $ solveEquations input regularNotation

solvePt2 :: String -> [String] -> IO String
solvePt2 input _args = do
  pure $ solveEquations input cephalopodNotation

main :: IO ()
main = do
  -- 🎅🎄❄️☃️🎁🦌
  -- Bright christmas lights HERE
  v1Run solvePt1 solvePt2
