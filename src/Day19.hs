module Day19 ( solution ) where

import Common (Solution(Solution), NoSolution(..), readNum)
import Data.List.Split (splitOn)
import Control.Monad.State (evalState, State, get)
import qualified Data.Map.Strict as M
import qualified Data.Set as S

solution = Solution "day19" run

run input = (NoSolution, NoSolution) -- let parsed = parse input in (part1 parsed, NoSolution)

-- parse :: String -> ([String], [String])
-- parse input = (towels, designs) where
--     [towelsStr, designsStr] = splitOn "\n\n" input
--     towels = splitOn ", " towelsStr
--     designs = splitOn "\n" designsStr

-- part1 :: ([String], [String]) -> Int
-- part1 (towels, designs) = length $ filter (validDesign towels) designs

-- validDesign :: [String] -> String -> Bool
-- validDesign towels design = evalState (validDesign' (S.fromList towels) design) M.empty

-- validDesign' :: [String] -> String -> State (M.Map ([String], String) Bool) Bool
-- validDesign' [] [] = return True
-- validDesign' [] _ = return False
-- validDesign' towels design = do
--     cache <- get
--     let alreadyCalculated = M.lookup (towels, design) cache
--     case alreadyCalculated of
--         Just value -> return value
--         _ -> undefined where
--             towels' = map (`isPrefixOf` design) towels
--             validTowels =
