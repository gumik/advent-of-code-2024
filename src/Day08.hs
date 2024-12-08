module Day08 ( solution, combinations ) where

import Common (Solution(Solution), NoSolution(..), readNum, parseArray, toTuple)
import Data.Array (assocs, bounds)
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Data.Tuple (swap)
import Debug.Trace (traceShow)

solution = Solution "day08" run

run input = let antennas = parse input in (part1 antennas, part2 antennas)

type Antenna = ((Int, Int), Char)

parse :: String -> ((Int, Int), [Antenna])
parse input = let
    arr = parseArray id input
    points = assocs arr
    ((a, b), (height, width)) = bounds arr
    in ((height, width), filter ((/= '.') . snd) points)

part1 :: ((Int, Int), [Antenna]) -> Int
part1 = calculateAntinodes antinodesForAntennasPair

part2 :: ((Int, Int), [Antenna]) -> Int
part2 = calculateAntinodes antinodesForAntennasPair2

calculateAntinodes :: AntinodesForPair -> ((Int, Int), [Antenna]) -> Int
calculateAntinodes antinodesForPair (arrBounds, antennas) = length antinodes where
    grouppedAntennas = groupAntennas antennas
    antinodes = S.toList $ S.fromList $ concatMap (antinodesForAntennasGroup antinodesForPair arrBounds . snd) grouppedAntennas

groupAntennas :: [Antenna] -> [(Char, [(Int, Int)])]
groupAntennas antennas = M.toList $ M.fromListWith (++) (map (\(p, c) -> (c, [p])) antennas)

type AntinodesForPair = (Int, Int) -> ((Int, Int), (Int, Int)) -> [(Int, Int)]

antinodesForAntennasGroup :: AntinodesForPair -> (Int, Int) -> [(Int, Int)] -> [(Int, Int)]
antinodesForAntennasGroup antinodesForPair arrBounds group = concatMap (antinodesForPair arrBounds) pairs where
    pairs = map toTuple $ combinations 2 group

antinodesForAntennasPair :: AntinodesForPair
antinodesForAntennasPair arrBounds ((y1, x1), (y2, x2)) = filter (inBounds arrBounds) [(y1 + dy, x1 + dx), (y2 - dy, x2 - dx)] where
    dy = y1 - y2
    dx = x1 - x2

antinodesForAntennasPair2 :: AntinodesForPair
antinodesForAntennasPair2 (ym, xm) ((y1, x1), (y2, x2)) = antinodes1 ++ antinodes2 where
    (dy, dx) = (y1 - y2, x1 - x2)
    antinodes1 = takeWhile  (inBounds (ym, xm)) $ iterate (\(y, x) -> (y + dy, x + dx)) (y1, x1)
    antinodes2 = takeWhile  (inBounds (ym, xm)) $ iterate (\(y, x) -> (y - dy, x - dx)) (y2, x2)

combinations :: Int -> [a] -> [[a]]
combinations _ [] = []
combinations 1 xs = map (:[]) xs
combinations n (x:xs) = map (x:) (combinations (n-1) xs) ++ combinations n xs

inBounds :: (Int, Int) -> (Int, Int) -> Bool
inBounds (height, width) (y, x) = y >= 0 && y <= height && x >= 0 && x <= width
