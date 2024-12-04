{-# LANGUAGE TupleSections #-}
module Day04 ( solution, getLines, getLine ) where

import Common (Solution(Solution), NoSolution(..), readNum, parseArray, inArrayBounds, search)
import Data.Array (Array, bounds, (!))
import Prelude hiding (lines, getLine)
import Data.Map.Strict (fromListWith, toList)

solution = Solution "day04" run

run input = let board = parse input in (part1 board, part2 board)

type Point = (Int, Int)
type Direction = (Int, Int)
(←) = (0, -1)
(↑) = (-1, 0)
(→) = (0, 1)
(↓) = (1, 0)
(↖) = (-1, -1)
(↗) = (-1, 1)
(↘) = (1, 1)
(↙) = (1, -1)
type Board = Array Point Char

parse :: String -> Board
parse = parseArray id

part1 :: Board -> Int
part1 board = length $ concatMap (\l -> search compareSlice l "XMAS") (lines board ++ diagonals board)

part2 :: Board -> Int
part2 board = let
    -- all the found slices of MAS keyword
    slices = concatMap (\l -> search compareSlice l "MAS") (diagonals board)
    -- Extract the middle point of each and count by them.
    -- Points with count of 2 are what we are looking for.
    middlePoints = map (snd . (!! 1)) slices
    counts = fromListWith (+) (map (,1) middlePoints)
    in length $ filter (== 2) $ map snd $ toList counts

lines :: Board -> [Slice]
lines board = getLines board [(y, x0) | y <- [y0..ym]] (→)  -- horizontals
           ++ getLines board [(y, xm) | y <- [y0..ym]] (←)  -- horizontals backwards
           ++ getLines board [(y0, x) | x <- [x0..xm]] (↓)  -- verticals
           ++ getLines board [(ym, x) | x <- [x0..xm]] (↑)  -- verticals backwards
    where
        ((y0, x0), (ym, xm)) = bounds board
        y1 = y0+1

diagonals :: Board -> [Slice]
diagonals board =
          -- diagonals to bottom right
              getLines board [(y0, x) | x <- [x0..xm]] (↘)   -- from top
           ++ getLines board [(y, x0) | y <- [y1..ym]] (↘)   -- from left

          -- diagonals to bottom left
           ++ getLines board [(y0, x) | x <- [x0..xm]] (↙)   -- from top
           ++ getLines board [(y, xm) | y <- [y1..ym]] (↙)   -- from right

          -- diagonals to top right
           ++ getLines board [(ym, x) | x <- [x0..xm]]   (↗) -- from bottom
           ++ getLines board [(y, x0) | y <- [y0..ym-1]] (↗) -- from left

          -- diagonals to top left
           ++ getLines board [(ym, x) | x <- [x0..xm]]   (↖) -- from bottom
           ++ getLines board [(y, xm) | y <- [y0..ym-1]] (↖) -- from right
    where
        ((y0, x0), (ym, xm)) = bounds board
        y1 = y0+1

-- Generates list of slices from the board.
-- Each slice contains letters and coordinates and start in
-- a particular point from the startingPoints list. Direction of the
-- slice is determined by direction param.
-- Example:
--  abcd
--  efgh
--  jkli
-- getLines board [(0, 1), (0, 2)] (1, 0)
-- output: ["bfk", "cgl"]
type SliceItem = (Char, Point)
type Slice = [SliceItem]

getLines :: Board -> [Point] -> Direction -> [Slice]
getLines board startingPoints direction = map (getLine board direction) startingPoints

getLine :: Board -> Direction -> Point -> Slice
getLine board direction startingPoint = map (\p -> (board ! p, p)) points where
    points = takeWhile (inArrayBounds board) $ iterate (advancePoint direction) startingPoint

advancePoint :: Direction -> Point -> Point
advancePoint (d0, d1) (y, x) = (y+d0, x+d1)

compareSlice :: SliceItem -> Char -> Bool
compareSlice (c, _) x = c == x