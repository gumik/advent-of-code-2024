module Day10 ( solution ) where

import Common (Solution(Solution), NoSolution(..), readNum, parseArray, readNum', inArrayBounds)
import Data.Array (Array, assocs, (!))
import Control.Monad.State (State(..), evalState, get, put)
import qualified Data.Map.Strict as M
import Data.Maybe (maybe)
import Data.List (nub)
import Debug.Trace (traceShow)
import Control.Monad.Extra (concatMapM)
solution = Solution "day10" run

run = solve . parse

type HeightMap = Array Pos Int
type Pos = (Int, Int)

parse :: String -> HeightMap
parse = parseArray readNum'

solve :: HeightMap -> (Int, Int)
solve m = (sum $ map fst scores, sum $ map snd scores) where
    trailheads = map fst $ filter ((== 0) . snd) $ assocs m
    scores = evalState (mapM (trailheadScore m) trailheads) initialCache
    initialCache = M.fromList $ [(pos, ([pos], 1)) | (pos, height) <- assocs m, height == 9]

type Cache = M.Map Pos ([Pos], Int)
-- The cache value is a pair of:
--  * list of unique 9s (tops) reachable
--  * number of different paths to the 9s

trailheadScore :: HeightMap -> Pos -> State Cache (Int, Int)
trailheadScore hm trailhead = do
    (reachableTops, numberOfPaths) <- paths hm trailhead
    return (length reachableTops, numberOfPaths)

paths :: HeightMap -> Pos -> State Cache ([Pos], Int)
paths hm pos@(y, x) = do
    cache <- get
    case M.lookup pos cache of
        Just value -> return value
        Nothing -> do
            let height = hm ! pos
                neighbours = filter (\p -> inArrayBounds hm p && hm ! p == height + 1) [(y+1, x), (y, x+1), (y-1, x), (y, x-1)]
            reachableTops <- mapM (paths hm) neighbours
            let reachableTops' = nub $ concatMap fst reachableTops
                numberOfPaths = sum $ map snd reachableTops
            put $ M.insert pos (reachableTops', numberOfPaths) cache
            return (reachableTops', numberOfPaths)
