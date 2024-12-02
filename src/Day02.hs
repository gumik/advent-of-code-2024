module Day02 ( solution ) where

import Common (Solution(Solution), NoSolution(..), readNum, listOfNumbers)
import Data.List.Split (splitOn)
import Data.List (inits, tails)

solution :: Solution Int Int
solution = Solution "day02" run

run :: [Char] -> (Int, Int)
run input = let reports = parse input in (part1 reports, part2 reports)

type Report = [Level]
type Level = Int

parse :: String -> [Report]
parse = map (map readNum . splitOn " ") . lines

part1 :: [Report] -> Int
part1 = length . filter isSafe

part2 :: [Report] -> Int
part2 = length . filter isSafeWithOneToleration


isSafe :: Report -> Bool
isSafe report = isMonotonic && areChangesWithinRange where
    differences = zipWith (-) report (tail report)
    isMonotonic = all (< 0) differences || all (> 0) differences
    areChangesWithinRange = all ((\x -> x >= 1 && x <= 3) . abs) differences

isSafeWithOneToleration :: Report -> Bool
isSafeWithOneToleration report = any isSafe subReports where
    subReports = report : zipWith (++) (inits report) (tails (tail report))
