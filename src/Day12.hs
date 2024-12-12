{-# LANGUAGE TupleSections #-}
module Day12 ( solution, simplifyLines ) where

import Common (Solution(Solution), NoSolution(..), readNum, parseArray, inArrayBounds, showCharArray)
import Data.Array (Array, (!), indices, (//), listArray)
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Control.Monad.State
import Debug.Trace (traceShow, trace)

solution = Solution "day12" run

run input = let farm = parse input in (part1 farm, part2 farm)

type Point = (Int, Int)
type Farm = Array Point Char

parse :: String -> Farm
parse = parseArray id

part1 :: Farm -> Int
part1 farm = sum $ map (regionPrice farm) (findRegions farm)

part2 :: Farm -> Int
part2 farm = sum $ map (regionPriceDiscount farm) (findRegions farm)

type Region = (Char, S.Set Point)

findRegions :: Farm -> [Region]
findRegions farm = evalState (findRegions' farm) (S.fromList $ indices farm)

-- the state is not-visited points
findRegions' :: Farm -> State (S.Set Point) [Region]
findRegions' farm = do
    notVisited <- get
    if S.size notVisited == 0
        then return []
        else do
            let firstNotVisited = head $ S.toList notVisited
                plantId = farm ! firstNotVisited
                region = findRegion farm firstNotVisited
            put $ notVisited S.\\ region
            regions <- findRegions' farm
            return $ (plantId, region) : regions

findRegion :: Farm -> Point -> S.Set Point
findRegion farm point = snd $ head $ dropWhile (not . null . fst) $ iterate iteration ([point], S.empty) where
    iteration ([], region) = ([], region)
    iteration (point:points, region) = if point `S.member` region
                                        then (points, region)
                                        else (regionNeighbours farm point ++ points, point `S.insert` region)

regionNeighbours :: Farm -> Point -> [Point]
regionNeighbours farm p = filter ((== plantId) . (farm !)) $ neighbours farm p where
    plantId = farm ! p

neighbours :: Farm -> Point -> [Point]
neighbours farm p@(y, x) = filter (inArrayBounds farm) points where
    points = [(y-1, x), (y, x-1), (y+1, x), (y, x+1)]

regionPrice :: Farm -> Region -> Int
regionPrice farm (_, points) = area * perimeter where
    area = S.size points
    perimeter = sum $ map (\p -> 4 - length (regionNeighbours farm p)) (S.toList points)

-- Scan the region line by line horizontally, then vertically and sum continous fences.
-- eg.
--  1 1 0 2 0 1 1
--   A A A A A A  1
--   A A A     A  1
--   A A A     A  0
--   A     A A A  2
--   A     A A A  0
--   A A A A A A  1
--                1
regionPriceDiscount :: Farm -> Region -> Int
regionPriceDiscount farm (c, points) = length points' * (edges horizontalLines + edges verticalLines) where
    points' = S.toList points
    minX = minimum $ map snd points'
    maxX = maximum $ map snd points'
    minY = minimum $ map fst points'
    maxY = maximum $ map fst points'
    regionArr = listArray ((minY-1, minX-1), (maxY+1, maxX+1)) (repeat '.') // map (,c) points'
    horizontalLines = map (\y -> [regionArr ! (y, x)| x <- [minX..maxX]]) [minY-1..maxY+1]
    verticalLines = map (\x -> [regionArr ! (y, x)| y <- [minY..maxY]]) [minX-1..maxX+1]

edges :: [[Char]] -> Int
edges lines = sum $ zipWith edgesForTwoLines lines (tail lines)

edgesForTwoLines :: [Char] -> [Char] -> Int
edgesForTwoLines l1 l2 = length $ filter (uncurry (/=)) $ simplifyLines l1 l2

-- merges continous sequences of the same values
simplifyLines :: [Char] -> [Char] -> [(Char, Char)]
simplifyLines l1 l2 = case (l1, l2) of
                        ([], [])   -> []
                        ([c1], [c2]) -> [(c1, c2)]
                        ('.':xs, '.':ys) -> simplifyLines xs ys
                        (x1:x2:xs, y1:y2:ys) -> if x1 == x2 && y1 == y2
                                                then simplifyLines (x2:xs) (y2:ys)
                                                else (x1, y1) : simplifyLines (x2:xs) (y2:ys)
