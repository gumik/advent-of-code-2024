module Day16 ( solution ) where

import Common (Solution(Solution), NoSolution(..), readNum, parseArray)
import Data.Array (assocs)
import Data.Bifunctor
import Data.Tuple (swap)
import Debug.Trace (traceShow)
import qualified Data.Set as S

solution = Solution "day16" run

run input = let board = parse input in (NoSolution, NoSolution)

type Point = (Int, Int)
type Start = Point
type End = Point

parse :: String -> (Start, End, [Point])
parse input = (start, end, map fst points) where
    arr = parseArray id input
    points = map (first swap) $ filter ((/= '#') . snd) $ assocs arr
    start = fst $ head $ filter ((== 'S') . snd) points
    end = fst $ head $ filter ((== 'E') . snd) points


-- simple brute-force (finding all possible paths) is too slow
-- BFS with weighted-edges probably would do

-- part1 :: (Start, End, [Point]) -> Int
-- part1 (start, end, points) = traceShow paths 0 where -- minimum $ map pathScore paths where
--     paths = findPaths end (S.fromList points) S.empty start

-- pathScore :: [Point] -> Int
-- pathScore = undefined

-- findPaths :: End -> S.Set Point -> S.Set Point -> Point -> [[Point]]
-- findPaths end points visited point@(x,y)
--     | end == point  = [[end]]
--     | otherwise     = concatMap (findPaths end points visited') neighbours where
--         neighbours = filter (\p -> p `S.member` points && p `S.notMember` visited) [(x+1, y), (x, y+1), (x-1, y), (x, y-1)]
--         visited' = point `S.insert` visited
