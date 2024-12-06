module Day06 ( solution ) where

import Common (Solution(Solution), NoSolution(..), readNum, parseArray, inArrayBounds)
import Data.Array (Array, assocs, (!), (//))
import qualified Data.Set as S
import Debug.Trace (traceShow)

solution = Solution "day06" run

run input = (part1 path, part2 situationMap path) where
    situationMap = parse input
    path = guardPath situationMap

data Position = Empty | Obstacle | Guard deriving Eq
type Point = (Int, Int)
type SituationMap = Array Point Position

parse :: String -> SituationMap
parse = parseArray toPosition

toPosition :: Char -> Position
toPosition x = case x of
    '.' -> Empty
    '#' -> Obstacle
    '^' -> Guard


part1 :: [(Point, Direction)] -> Int
part1 = S.size . S.fromList . map fst

guardPath :: SituationMap -> [(Point, Direction)]
guardPath situationMap = genPath situationMap' (startPoint, (-1, 0)) where
    startPoint = fst $ head $ filter ((== Guard) . snd) $ assocs situationMap
    situationMap' = fmap (\x -> if x == Guard then Empty else x) situationMap

type Direction = (Int, Int)

genPath :: SituationMap -> (Point, Direction) -> [(Point, Direction)]
genPath situationMap (point, direction) = let
    nextPos = moveOne point direction
    in if inArrayBounds situationMap nextPos then
        case situationMap ! nextPos of
            Empty -> (point, direction) : genPath situationMap (nextPos, direction)
            Obstacle -> genPath situationMap (point, rotateRight direction)
       else [(point, direction)]


moveOne :: Point -> Direction -> Point
moveOne (x, y) (x1, y1) = (x + x1, y + y1)

rotateRight :: Direction -> Direction
rotateRight (y, x) = (x, -y)

-- Try adding obstacle on every point of the guard path.
-- Then check how many ends up with cycle.
part2 :: SituationMap -> [(Point, Direction)] -> Int
part2 situationMap path = length $ filter isCycle $ map guardPath situationMapsWithAddedObstacles where
    pathPoints = S.toList $ S.fromList $ map fst path
    obstaclesPoints = filter ((/= Guard) . (situationMap !)) pathPoints
    situationMapsWithAddedObstacles = map (\p -> situationMap // [(p, Obstacle)]) obstaclesPoints

isCycle :: [(Point, Direction)] -> Bool
isCycle = isCycle' S.empty where
    isCycle' _ [] = False
    isCycle' visited (x:xs) = S.member x visited  ||  isCycle' (S.insert x visited) xs
