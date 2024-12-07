module Day07 ( solution, calculate, isValid, genPossibilities ) where

import Common (Solution(Solution), NoSolution(..), readNum)
import Data.List.Split (splitOn)
import Debug.Trace (traceShow)

solution = Solution "day07" run

run input = let equations = parse input in (part1 equations, part2 equations)

type Equation = (Int, [Int])

parse :: String -> [Equation]
parse = map parseLine . lines

parseLine :: String -> Equation
parseLine input = (testValue, numbers) where
    [testValueStr, rest] = splitOn ": " input
    testValue = readNum testValueStr
    numbers = map readNum $ splitOn " " rest

part1 :: [Equation] -> Int
part1 = sum . map fst . filter (isValid [(*), (+)])

part2 :: [Equation] -> Int
part2 = sum . map fst . filter (isValid [(*), (+), (|-|)])

-- TODO: optimize to not calculate further when number is bigger
-- Or maybe totaly different approach?
-- Try one operator the recursively find in subsequence if can produce testValue-value?
-- Wouldn't that be the same??
isValid :: [Int -> Int -> Int] -> Equation -> Bool
isValid ops (testValue, numbers) = {- traceShow (testValue, numbers, result) -} result where
    result = any ((== testValue) . calculate numbers) possibilities
    possibilities = genPossibilities ops (length numbers - 1)

-- TODO: maybe use List monad?
genPossibilities :: [a] -> Int -> [[a]]
genPossibilities ops 0 = [[]]
genPossibilities ops n = concatMap (\op -> map (op :) (genPossibilities ops (n - 1))) ops

calculate :: [Int] -> [Int -> Int -> Int] -> Int
-- calculate (n:ns) ops = foldl (\(acc, (b, op)) -> acc `op` b) n (ns `zip` ops)
calculate nums ops = calculate' nums ops where
    calculate' [a, b] [op] = a `op` b
    calculate' (a:b:as) (op:ops) = calculate (a `op` b : as) ops

(|-|) :: Int -> Int -> Int
a |-| b = readNum $ show a ++ show b
