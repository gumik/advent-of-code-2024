{-# LANGUAGE TupleSections #-}
module Day11 ( solution, blink, blinkOne ) where

import Common (Solution(Solution), NoSolution(..), readNum)
import Data.List.Split (splitOn)
import Control.Monad.State (State(..), evalState, get, put)
import Data.Maybe (fromMaybe)
import Control.Monad.Extra (concatMapM)
import qualified Data.Map.Strict as M

solution = Solution "day11" run

run input = let stones = parse input in (part1 stones, part2 stones)

parse :: String -> [Int]
parse = map readNum . splitOn " "

part1 :: [Int] -> Int
part1 = blink 25

part2 :: [Int] -> Int
part2 = blink 75

blink :: Int -> [Int] -> Int
blink n stones = sum $ map snd $ M.toList $ last $ take (n+1) $ iterate blinkOne (M.fromList (map (,1) stones))

blinkOne :: M.Map Int Int -> M.Map Int Int
blinkOne stones =  M.fromListWith (+) $ concatMap (\(stone, count) -> map (,count) (iterateStone stone)) (M.toList stones)

iterateStone :: Int -> [Int]
iterateStone 0 = [1]
iterateStone x = fromMaybe [x*2024] (splitStone x)

splitStone :: Int -> Maybe [Int]
splitStone stone = let
    str = show stone
    (half, remainder) = length str `divMod` 2
  in case remainder of
    0 -> Just [readNum $ take half str, readNum $ drop half str]
    _ -> Nothing
