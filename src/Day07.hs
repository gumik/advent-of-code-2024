module Day07 ( solution, isValid, Operator(..) ) where

import Common (Solution(Solution), NoSolution(..), readNum)
import Data.List.Split (splitOn)
import Debug.Trace (traceShow)
import Data.Maybe (mapMaybe)

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
part1 = sum . map fst . filter (isValid [Plus, Multiply])

part2 :: [Equation] -> Int
part2 = sum . map fst . filter (isValid [Plus, Multiply, Concat])

data Operator = Plus | Multiply | Concat

-- Go number by number from the back of the list.
-- Try all the possible operations, but reverse it and check recursively for changed testValue and rest of the list.
-- Example:
-- (9, [1, 2, 3])
-- 1. (+) -> (6, [1, 2])
--   1.1 (+) -> (4, [1])
--     1.1.1 (+) -> (3, []) -> False
--     1.1.2 (*) -> (4, []) -> False
--   1.2 (*) -> (3, [1])
--     1.2.1 (+) -> (2, []) -> False
--     1.2.2 (*) -> (3, []) -> False
-- 2. (*) -> (3, [1, 2])
--   2.1 (+) -> (1, [1])
--     2.1.1 (+) -> (0, []) -> True
--   2.2 (*) -> not possible (3/2 not divisible)
-- There was one branch which resulted in empty list and testValue=0 so the answer is True.
isValid :: [Operator] -> Equation -> Bool
isValid ops (testValue, numbers) = isValid' ops (reverse numbers) testValue

isValid' :: [Operator] -> [Int] -> Int -> Bool
isValid' _ [] 0 = True
isValid' _ [] _ = False
isValid' ops (x:xs) testValue = any (isValid' ops xs) (mapMaybe (opInverse testValue x) ops)


-- Concatenation of eg. 3 is in fact equal to (* 10 + 3).
-- The inverse of it is (- 3 / 10).
opInverse :: Int -> Int -> Operator -> Maybe Int
opInverse testValue x Plus = let testValue' = testValue - x in if testValue' < 0 then Nothing else Just testValue'
opInverse testValue x Multiply = let (testValue', rest) = testValue `divMod` x in if rest /= 0 then Nothing else Just testValue'
opInverse testValue x Concat = do
    afterSub <- opInverse testValue x Plus
    opInverse afterSub (10 ^ length (show x)) Multiply

