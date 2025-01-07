module Day11 ( solution, blink, blinkOne ) where

import Common (Solution(Solution), NoSolution(..), readNum)
import Data.List.Split (splitOn)
import Control.Monad.State (State(..), evalState)
import Data.Maybe (fromMaybe)
import Control.Monad.Extra (concatMapM)

solution = Solution "day11" run

run input = let stones = parse input in (part1 stones, NoSolution)

parse :: String -> [Int]
parse = map readNum . splitOn " "

part1 :: [Int] -> Int
part1 = blink 25

-- for part1, no-cache has ok speed
data Cache = Cache

-- Give either:
--   * Right: number of stones after given number of blinks.
--   * Left: stones after less number of blinks than given and that number of blinks.
readCache :: Int -> Int -> State Cache (Either ([Int], Int) Int)
readCache n stone = return $ Left ([stone], 0)

writeCache :: Int -> [Int] -> State Cache ()
writeCache n stones = return ()

blink :: Int -> [Int] -> Int
blink n stones = sum $ evalState (mapM (blinkOne n) stones) Cache

blinkOne :: Int -> Int -> State Cache Int
blinkOne 0 _ = return 1
blinkOne n stone = do
    cacheItem <- readCache n stone
    case cacheItem of
        Right numberOfStones -> return numberOfStones
        Left (stones, m) -> do
            let newStones = map iterateStone stones
            mapM_ (writeCache (m+1)) newStones
            stones' <- mapM (blinkOne (n - m - 1)) (concat newStones)
            return $ sum stones'

iterateStone :: Int -> [Int]
iterateStone 0 = [1]
iterateStone x = fromMaybe [x*2024] (splitStone x)

splitStone :: Int -> Maybe [Int]
splitStone stone = let
    str = show stone
    (half, remainder) = length str `divMod` 2
  in case remainder of
    0 -> Just [readNum $ take half str, readNum $ drop half str]
    _ -> Nothing
