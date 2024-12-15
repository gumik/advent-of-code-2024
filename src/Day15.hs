{-# LANGUAGE TupleSections #-}
module Day15 ( solution, tryMove, Item(..), Move(..)) where

import Common (Solution(Solution), NoSolution(..), readNum, parseArray, showCharArray)
import Data.List.Split (splitOn)
import Data.Array (assocs, (//), listArray)
import Data.Tuple (swap)
import Data.Bifunctor (second, first, Bifunctor (bimap))
import Data.Maybe (isJust, fromJust)
import Prelude hiding (Right, Left)
import qualified Data.Map.Strict as M
import Debug.Trace (traceShow, trace)

solution = Solution "day15" run

run input = let parsed = parse input in (part1 parsed, NoSolution)

type Point = (Int, Int)
data Item = Box | WideBox Int | Wall deriving Show -- | Empty  -- WideBox value is its left X coordinate
data Move = Left | Up | Right | Down

isBox :: Item -> Bool
isBox Box = True
isBox _ = False

parse :: String -> (Point, [(Point, Item)], [Move])
parse input = (robotPos, items, moves) where
    [warehouseStr, movesStr] = splitOn "\n\n" input
    arr = parseArray id warehouseStr
    robotPos = swap $ fst $ head $ filter ((== '@') . snd) (assocs arr)
    items = map (bimap swap fromJust) $ filter (isJust . snd) $ map (second parseItem) (assocs arr)
    moves = map parseMove $ filter (/= '\n') movesStr

parseItem :: Char -> Maybe Item
parseItem c = case c of
    '#' -> Just Wall
    'O' -> Just Box
    _   -> Nothing

parseMove :: Char -> Move
parseMove c = case c of
    '<' -> Left
    '^' -> Up
    '>' -> Right
    'v' -> Down

part1 :: (Point, [(Point, Item)], [Move]) -> Int
part1 (robotPos, items, moves) = {- trace (concatMap (\(rp, its) -> "\n" ++ showBoard rp its) xx) $ -} sum $ map (gpsCoord . fst) $ filter (isBox . snd) (M.toList items') where
    xx = scanl makeMove (robotPos, M.fromList items) moves
    items' = snd $ last xx

gpsCoord :: Point -> Int
gpsCoord (x, y) = 100*y + x

makeMove :: (Point, M.Map Point Item) -> Move -> (Point, M.Map Point Item)
makeMove (robotPos, items) move = case tryMove items robotPos move of
        Nothing -> (robotPos, items)
        Just boxMoves -> (robotPos', items') where
            robotPos' = nextPos move robotPos
            itemsRemovedFrom = foldl (\its (from, _) -> M.delete from its) items boxMoves
            itemsAddedTo = foldl (\its (_, to) -> M.insert to Box its) itemsRemovedFrom boxMoves
            items' = itemsAddedTo

{-
Checks if it is possible to move robot from given point to given direction.
Detects collisions with boxes and walls. If the move is not possible then result is Nothing.
If it is possible it is Just with a list of all the boxes movements that are pushed by the robot.
Item in the list is (from, to).
-}
tryMove :: M.Map Point Item -> Point -> Move -> Maybe [(Point, Point)]
tryMove items pos move = let
        currentItem = M.lookup pos items
        pos' = nextPos move pos
        currentItemMove = maybe [] (const [(pos, pos')]) (M.lookup pos items) in
    case M.lookup pos' items of
        Nothing -> Just currentItemMove
        Just Wall -> Nothing
        Just Box -> do
            otherBoxesToMove <- tryMove items pos' move
            return $ currentItemMove ++ otherBoxesToMove

nextPos :: Move -> Point -> Point
nextPos move (x, y) = case move of
    Left  -> (x-1, y)
    Up    -> (x, y-1)
    Right -> (x+1, y)
    Down  -> (x, y+1)

showBoard :: Point -> M.Map Point Item -> String
showBoard robot items = showCharArray arr where
    items' = map (first swap) $ M.toList items
    width = maximum (map (fst . fst) items')
    height = maximum (map (snd . fst) items')
    arr = listArray ((0, 0), (height, width)) (repeat ' ') // map (second itemToChar) items' // [(swap robot, '@')]

itemToChar Box = 'O'
itemToChar Wall = '#'