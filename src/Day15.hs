{-# LANGUAGE TupleSections #-}
module Day15 ( solution, tryMove, Item(..), Move(..)) where

import Common (Solution(Solution), NoSolution(..), readNum, parseArray, showCharArray)
import Data.List.Split (splitOn)
import Data.Array (assocs, (//), listArray)
import Data.Tuple (swap)
import Data.Bifunctor (second, first, Bifunctor (bimap))
import Data.Maybe (isJust, fromJust, mapMaybe)
import Prelude hiding (Right, Left)
import qualified Data.Map.Strict as M
import Debug.Trace (traceShow, trace)
import Control.Monad.Extra (concatMapM)

solution = Solution "day15" run

run input = (solve (robotPos, items, moves), solve (robotPos', items', moves)) where
    [warehouseStr, movesStr] = splitOn "\n\n" input
    (robotPos, items) = parseWarehouse warehouseStr
    (robotPos', items') = parseWarehouse $ extend warehouseStr
    moves = parseMoves movesStr

type Point = (Int, Int)
data Item = Box | WideBox Int | Wall deriving Show -- | Empty  -- WideBox value is its left X coordinate
data Move = Left | Up | Right | Down

parseWarehouse :: String -> (Point, [(Point, Item)])
parseWarehouse warehouseStr = (robotPos, items) where
    arr = parseArray id warehouseStr
    robotPos = swap $ fst $ head $ filter ((== '@') . snd) (assocs arr)
    items = map (first swap) $ mapMaybe parseItem (assocs arr)

parseMoves :: String -> [Move]
parseMoves str = map parseMove $ filter (/= '\n') str

parseItem :: (Point, Char) -> Maybe (Point, Item)
parseItem (p@(y, x), c) = case c of
    '#' -> Just (p, Wall)
    'O' -> Just (p, Box)
    '[' -> Just (p, WideBox x)
    ']' -> Just (p, WideBox (x-1))
    _   -> Nothing

parseMove :: Char -> Move
parseMove c = case c of
    '<' -> Left
    '^' -> Up
    '>' -> Right
    'v' -> Down

extend :: String -> String
extend = concatMap extendElem

extendElem :: Char -> String
extendElem c = case c of
    '#' -> "##"
    'O' -> "[]"
    '@' -> "@."
    '.' -> ".."
    '\n' -> "\n"

solve :: (Point, [(Point, Item)], [Move]) -> Int
solve (robotPos, items, moves) = sum $ map (gpsCoord . fst) $ filter isBox (M.toList items') where
    xx = scanl makeMove (robotPos, M.fromList items) moves
    items' = snd $ last xx

isBox :: (Point, Item) -> Bool
isBox (_, Box) = True
isBox ((x, _), WideBox wx) = x == wx
isBox _ = False

gpsCoord :: Point -> Int
gpsCoord (x, y) = 100*y + x

makeMove :: (Point, M.Map Point Item) -> Move -> (Point, M.Map Point Item)
makeMove (robotPos, items) move = case tryMove items (nextPos move robotPos) move of
        Nothing -> (robotPos, items)
        Just boxMoves -> (robotPos', items') where
            robotPos' = nextPos move robotPos
            itemsRemovedFrom = foldl (\its (from, _, _) -> M.delete from its) items boxMoves
            itemsAddedTo = foldl (\its (_, to, item) -> M.insert to item its) itemsRemovedFrom boxMoves
            items' = itemsAddedTo

{-
Checks if it is possible to move box from given point to given direction.
Detects collisions with other boxes and walls. If the move is not possible then result is Nothing.
If it is possible it is Just with a list of all the boxes movements that are pushed.
Item in the list is (from, to).
-}
tryMove :: M.Map Point Item -> Point -> Move -> Maybe [(Point, Point, Item)]
tryMove items pos@(x, y) move =
    case  M.lookup pos items of
        Nothing -> Just []
        Just Wall -> Nothing
        Just Box -> do
            let pos' = nextPos move pos
            otherBoxesToMove <- tryMove items (nextPos move pos) move
            return $ (pos, pos', Box) : otherBoxesToMove
        Just (WideBox x1) -> do
            let boxesPoints = [(x1, y), (x1+1, y)]
                nextBoxesPoints = map (nextPos move) boxesPoints
                nextPoints = filter (not . (`elem` boxesPoints)) nextBoxesPoints
                nextBoxX = fst $ head nextBoxesPoints
            otherBoxesToMove <- concatMapM (\p -> tryMove items p move) nextPoints
            return $ zip3 boxesPoints nextBoxesPoints (repeat $ WideBox nextBoxX) ++ otherBoxesToMove

nextPos :: Move -> Point -> Point
nextPos move (x, y) = case move of
    Left  -> (x-1, y)
    Up    -> (x, y-1)
    Right -> (x+1, y)
    Down  -> (x, y+1)


-- debug stuff below
showBoard :: Point -> M.Map Point Item -> String
showBoard robot items = showCharArray arr where
    items' = map (first swap) $ M.toList items
    width = maximum (map (fst . fst) items')
    height = maximum (map (snd . fst) items')
    arr = listArray ((0, 0), (height, width)) (repeat ' ') // map (second itemToChar) items' // [(swap robot, '@')]

itemToChar Box = 'O'
itemToChar Wall = '#'