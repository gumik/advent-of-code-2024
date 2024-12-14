{-# LANGUAGE TupleSections #-}
module Day14 ( solution, part2 ) where

import Common (Solution(Solution), NoSolution(..), readNum, toTuple, parseComaSeparatedNums, showCharArray)
import Data.List.Split (splitOn)
import Data.Char (isDigit)
import Data.Array (listArray, (//))
import qualified Data.Set as S

solution = Solution "day14" run

run input = let parsed = parse input in (part1 parsed, NoSolution)

type Position = (Int, Int)
type Velocity = (Int, Int)
data Robot = Robot Position Velocity

robotPosX :: Robot -> Int
robotPosX (Robot (x, _) _) = x

robotPosY :: Robot -> Int
robotPosY (Robot (_, y) _) = y

parse :: String -> ((Int, Int), [Robot])
parse input = (parseSize bathroomSizeStr, map parseRobot robotsStr) where
    inputLines = lines input
    bathroomSizeStr = head inputLines
    robotsStr = tail inputLines

parseSize :: String -> (Int, Int)
parseSize str = (readNum x, readNum $ takeWhile isDigit y) where
    x:y:_ = splitOn "," str

parseRobot :: String -> Robot
parseRobot str = Robot pos vel where
    posStr:velStr:_ = splitOn " " str
    pos = toTuple $ parseComaSeparatedNums (drop 2 posStr)
    vel = toTuple $ parseComaSeparatedNums (drop 2 velStr)

part1 :: ((Int, Int), [Robot]) -> Int
part1 (size@(width, height), robots) = product $ map length quadrants where
    widthMiddle = width `div` 2
    heightMiddle = height `div` 2
    finalPositions = map (moveRobot size 100) robots
    quadrants = [
        filter (\robot -> robotPosX robot < widthMiddle && robotPosY robot < heightMiddle) finalPositions,
        filter (\robot -> robotPosX robot > widthMiddle && robotPosY robot < heightMiddle) finalPositions,
        filter (\robot -> robotPosX robot < widthMiddle && robotPosY robot > heightMiddle) finalPositions,
        filter (\robot -> robotPosX robot > widthMiddle && robotPosY robot > heightMiddle) finalPositions]

moveRobot :: (Int, Int) -> Int -> Robot -> Robot
moveRobot (width, height) n (Robot (x, y) (vx, vy)) = Robot ((x + vx*n) `mod` width, (y + vy*n) `mod` height) (vx, vy)

{-
Part 2 is a little bit unusual. It will use human as a christmas tree pattern detector.

Human can run it with the command: stack run -- day14part2
After running the human should observe printed patterns and hit CTRL+C as soon as
christmas tree pattern is detected.

As a help for human a christmasScore is defined for each iteration. The program will print
only the iterations which has greater christmasScore than previous max score.
The christmasScore is based on an assumption that a robots arranged into christmas tree are dense.
So, for each robot number of neighbouring robots are calculated. The sum of them is a christmasScore.
-}
part2 :: String -> IO ()
part2 input = do
    let (size, robots) = parse input
        boards = iterate (findTree size) (0, 0, 0, robots)
        boards' = filter (\(_, score, maxScore, _) -> score == maxScore) boards
    mapM_ (printBoard size) boards'

findTree :: (Int, Int) -> (Int, Int, Int, [Robot]) -> (Int, Int, Int, [Robot])
findTree size (i, score, maxScore, robots) = (i+1, score', max score' maxScore, robots') where
    robots' = map (moveRobot size 1) robots
    score' = christmasScore robots'

christmasScore :: [Robot] -> Int
christmasScore robots = sum $ map (neighboursCount robotsMap) robots where
    robotsMap = S.fromList [(x, y) | (Robot (x, y) _) <- robots]

neighboursCount :: S.Set (Int, Int) -> Robot -> Int
neighboursCount robotsMap (Robot (x, y) _) = length $ filter (`S.member` robotsMap) neighbours where
    neighbours = [(x-1, y), (x+1, y), (x, y-1), (x, y+1),
                  (x-1, y-1), (x-1, y+1), (x+1, y-1), (x+1, y+1)]

printBoard :: (Int, Int) -> (Int, Int, Int, [Robot]) -> IO ()
printBoard (width, height) (i, score, maxScore, robots) = do
    putStrLn $ "iteration: " ++ show i ++ ", score: " ++ show score
    let points = map (\(Robot (x, y) _) -> (y, x)) robots
        arr = listArray ((0, 0), (height, width)) (repeat ' ') // map (,'O') points
    putStrLn $ showCharArray arr
