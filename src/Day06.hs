module Day06 ( solution ) where

import Common (Solution(Solution), NoSolution(..), readNum, parseArray, inArrayBounds)
import Data.Array (Array, assocs, (!), (//), bounds)
import qualified Data.Set as S
import Debug.Trace (traceShow)
import Data.Tuple (swap)

solution = Solution "day06" run

run = solve . parse

data Position = Empty | Obstacle | Guard deriving Eq
type Point = (Int, Int)
type SituationMap = Array Point Position
type Direction = (Int, Int)

parse :: String -> SituationMap
parse = parseArray toPosition

toPosition :: Char -> Position
toPosition x = case x of
    '.' -> Empty
    '#' -> Obstacle
    '^' -> Guard

solve :: SituationMap -> (Int, Int)
solve situationMap = (part1, part2) where
    startPoint = fst $ head $ filter ((== Guard) . snd) $ assocs situationMap
    -- Sets of guard positions. By (y, x) and (x, y) coordinates. To quickly find next obstacle position.
    guardsYX = S.fromList $ map fst $ filter ((== Obstacle) . snd) $ assocs situationMap
    guardsXY = S.map swap guardsYX
    mapBounds = bounds situationMap
    path = guardPath mapBounds (guardsYX, guardsXY) startPoint
    pathPoints = S.toList $ S.fromList $ allPathPoints path
    part1 = length pathPoints

    -- For Part 2:
    -- Try adding obstacle on every point of the guard path.
    -- Then check how many ends up with cycle.
    obstaclesPoints = filter ((/= Guard) . (situationMap !)) pathPoints
    guards = map (\(y, x) -> (S.insert (y, x) guardsYX, S.insert (x, y) guardsXY)) obstaclesPoints
    part2 = length $ filter isCycle $ map (\guards -> guardPath mapBounds guards startPoint) guards

guardPath :: (Point, Point) -> (S.Set Point, S.Set Point) -> Point -> [(Point, Direction)]
guardPath mapBounds guards startPoint = genPath mapBounds guards (startPoint, (-1, 0))


genPath :: (Point, Point) -> (S.Set Point, S.Set Point) -> (Point, Direction) -> [(Point, Direction)]
genPath arrBounds@((y0, x0), (ym, xm)) (guardsYX, guardsXY) (point@(y, x), direction@(dy, dx)) = let
    (nextObstacle, lineCoord) = case direction of
        (0, 1)  -> (S.lookupGE (y+dy, x+dx) guardsYX, fst)
        (0, -1) -> (S.lookupLE (y+dy, x+dx) guardsYX, fst)
        (1, 0)  -> (swap <$> S.lookupGE (x+dx, y+dy) guardsXY, snd)
        (-1, 0) -> (swap <$> S.lookupLE (x+dx, y+dy) guardsXY, snd)
    in case nextObstacle of
        Nothing -> [(point, direction), edgePoint arrBounds (point, direction)]
        Just nextObstaclePos@(oy, ox) -> if lineCoord nextObstaclePos == lineCoord point
            then (point, direction) : genPath arrBounds (guardsYX, guardsXY) ((oy-dy, ox-dx), rotateRight direction)
            else [(point, direction), edgePoint arrBounds (point, direction)]

allPathPoints :: [(Point, Direction)] -> [Point]
allPathPoints points = concatMap extendLine (points' `zip` drop 1 points') where
    ((y, x), (dy, dx)) = last points
    points' = points ++ [((y+dy, x+dx), undefined)]

extendLine :: ((Point, Direction), (Point, Direction)) -> [Point]
extendLine ((p1@(y1, x1), (dy, dx)), (p2, _)) = takeWhile (/= p2) $ iterate (\(y, x) -> (y+dy, x+dx)) p1

edgePoint :: (Point, Point) -> (Point, Direction) -> (Point, Direction)
edgePoint arrBounds@((y0, x0), (ym, xm)) ((y, x), direction) = case direction of
    (1, 0) -> ((ym, x), direction)
    (-1, 0) -> ((y0, x), direction)
    (0, 1) -> ((y, xm), direction)
    (0, -1) -> ((y, x0), direction)

rotateRight :: Direction -> Direction
rotateRight (y, x) = (x, -y)

isCycle :: [(Point, Direction)] -> Bool
isCycle = isCycle' S.empty where
    isCycle' _ [] = False
    isCycle' visited (x:xs) = S.member x visited  ||  isCycle' (S.insert x visited) xs
