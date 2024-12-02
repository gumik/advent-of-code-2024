module Day01 ( solution ) where

import Common (Solution(Solution), NoSolution(..), readNum)
import Data.List.Split (splitOn)
import Data.List (sort)

solution :: Solution Int Int
solution = Solution "day01" run

run :: [Char] -> (Int, Int)
run input = let nums = parse input in (part1 nums, part2 nums)

parse :: String -> ([Int], [Int])
parse input = (map fst nums, map snd nums) where
    nums = map parseLine $ lines input

parseLine :: String -> (Int, Int)
parseLine line = let [x, y] = splitOn "   " line in (readNum x, readNum y)

part1 :: ([Int], [Int]) -> Int
part1 (leftList, rightList) = sum $ zipWith (\x y -> abs (x - y)) leftList' rightList' where
    leftList' = sort leftList
    rightList' = sort rightList

part2 :: ([Int], [Int]) -> Int
part2 (leftList, rightList) = sum $ map (similarityScore rightList) leftList

similarityScore :: [Int] -> Int -> Int
similarityScore list x = x * length (filter (==x) list)
