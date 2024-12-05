module Day05 ( solution, generateValidUpdate ) where

import Common (Solution(Solution), NoSolution(..), readNum, toTuple, parseComaSeparatedNums)
import Data.List.Split (splitOn)
import Control.Arrow ((***))
import Debug.Trace (traceShow)
import Data.Foldable (find)
import Data.Maybe (Maybe(Just, Nothing))

solution = Solution "day05" run

run input = (countMiddleElems validUpdates, countMiddleElems fixedUpdates) where
    (rules, updates) = parse input
    validUpdates = filter (validUpdate rules) updates
    invalidUpdates = filter (not . validUpdate rules) updates
    fixedUpdates = map (generateValidUpdate . rulesForUpdate rules) invalidUpdates

type Rules = [Rule]
type Rule = (Int, Int)
type Updates = [Update]
type Update = [Int]

parse :: String -> (Rules, Updates)
parse input = (parseRules rulesStr, parseUpdates updatesStr) where
    [rulesStr, updatesStr] = splitOn "\n\n" input

parseRules :: String -> Rules
parseRules = map ((readNum *** readNum) . toTuple . splitOn "|") . lines

parseUpdates :: String -> Updates
parseUpdates = map parseComaSeparatedNums . lines

countMiddleElems :: Updates -> Int
countMiddleElems updates = sum $ map middleElem updates

middleElem :: Update -> Int
middleElem update = update !! (length update `div` 2)

rulesForUpdate :: Rules -> Update -> Rules
rulesForUpdate rules update = filter (\(x, y) -> x `elem` update && y `elem` update) rules

{-
First, filter irrelevant rules, leave only the ones with numbers from update sequence.
Then go number by number in the update sequence and check if the number is in the rules on the right side.
If it is it means the sequence is invalid. Update rules after each iteration by removing all where
number is on the left side.
-}
validUpdate :: Rules -> Update -> Bool
validUpdate rules update = validUpdateIteration rules' update where
    rules' = rulesForUpdate rules update

validUpdateIteration :: Rules -> Update -> Bool
validUpdateIteration rules (x:xs) = notElem x (map snd rules) && validUpdateIteration rules' xs where
    rules' = filter ((/= x) . fst) rules
validUpdateIteration rules [] = True


-- Generates valid update from set of rules. This is basically sorting graph topologically.
generateValidUpdate :: Rules -> Update
generateValidUpdate [] = []
generateValidUpdate rules = case rules' of
        [] -> [startingRuleNumber, snd $ head rules] -- it means last rule was in the set; we need last element to be put in the result
        _ -> startingRuleNumber : generateValidUpdate rules'
    where
        -- find all the rules where left number is in no any other rule on the right side
        startingRules = filter (\(x, _) -> x `notElem` map snd rules) rules
        -- take the first one; if there is more than one then there is more than one solution - any is good
        startingRuleNumber = fst $ head startingRules
        -- new rule set does not contain rules where left side is equal to that number
        rules' = filter ((/= startingRuleNumber) . fst) rules
