module Day03 ( solution ) where

import Common (Solution(Solution), NoSolution(..), readNum)
import Text.Regex.Posix ((=~))

solution :: Solution Int Int
solution = Solution "day03" run

run :: String -> (Int, Int)
run input = (part1 input, part2 input)

part1 :: String -> Int
part1 input = sum $ map mulMatch matches where
    matches = input =~ "mul\\(([0-9]+),([0-9]+)\\)" :: [[String]]

mulMatch :: [String] -> Int
mulMatch [_, x, y] = readNum x * readNum y

part2 :: String -> Int
part2 input = snd $ foldl processItem (True, 0) matches where
    matches = input =~ "mul\\(([0-9]+),([0-9]+)\\)|do\\(\\)|don't\\(\\)" :: [[String]]

processItem :: (Bool, Int) -> [String] -> (Bool, Int)
processItem acc@(turnedOn, result) item = case item of
    ["do()", _, _] -> (True, result)
    ["don't()", _, _] -> (False, result)
    [_, x, y] -> if turnedOn then (turnedOn, result + readNum x * readNum y) else (turnedOn, result)
