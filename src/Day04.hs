module Day04 ( solution, getLines, getLine ) where

import Common (Solution(Solution), NoSolution(..), readNum, parseArray, inArrayBounds, search)
import Data.Array (Array, bounds, (!))
import Prelude hiding (getLine)

solution = Solution "day04" run

run input = let board = parse input in (part1 board, NoSolution)

type Point = (Int, Int)
type Direction = (Int, Int)
type Board = Array Point Char

parse :: String -> Board
parse = parseArray id

part1 :: Board -> Int
part1 board = length $ concatMap (`search` "XMAS") lines where
    ((y0, x0), (ym, xm)) = bounds board
    y1 = y0+1

    lines = getLines board [(y, x0) | y <- [y0..ym]] (0, 1)   -- horizontals
         ++ getLines board [(y, xm) | y <- [y0..ym]] (0, -1)  -- horizontals backwards
         ++ getLines board [(y0, x) | x <- [x0..xm]] (1, 0)   -- verticals
         ++ getLines board [(ym, x) | x <- [x0..xm]] (-1, 0)  -- verticals backwards

        -- diagonals to bottom right
         ++ getLines board [(y0, x) | x <- [x0..xm]] (1, 1)   -- from top
         ++ getLines board [(y, x0) | y <- [y1..ym]] (1, 1)   -- from left

        -- diagonals to bottom left
         ++ getLines board [(y0, x) | x <- [x0..xm]] (1, -1)  -- from top
         ++ getLines board [(y, xm) | y <- [y1..ym]] (1, -1)  -- from right

        -- diagonals to top right
         ++ getLines board [(ym, x) | x <- [x0..xm]] (-1, 1)    -- from bottom
         ++ getLines board [(y, x0) | y <- [y0..ym-1]] (-1, 1)  -- from left

        -- diagonals to top left
         ++ getLines board [(ym, x) | x <- [x0..xm]] (-1, -1)   -- from bottom
         ++ getLines board [(y, xm) | y <- [y0..ym-1]] (-1, -1) -- from right

-- Generates list of strings. Each string is a line in the board and starts
-- in a particular point from the startingPoints list. Direction of the
-- line is determined by direction param.
-- Example:
--  abcd
--  efgh
--  jkli
-- getLines board [(0, 1), (0, 2)] (1, 0)
-- output: ["bfk", "cgl"]
getLines :: Board -> [Point] -> Direction -> [String]
getLines board startingPoints direction = map (getLine board direction) startingPoints

getLine :: Board -> Direction -> Point -> String
getLine board direction startingPoint = map (board !) points where
    points = takeWhile (inArrayBounds board) $ iterate (advancePoint direction) startingPoint

advancePoint :: Direction -> Point -> Point
advancePoint (d0, d1) (y, x) = (y+d0, x+d1)
