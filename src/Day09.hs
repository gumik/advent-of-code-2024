{-# LANGUAGE TupleSections #-}
module Day09 ( solution, defrag, cutToSize, Block(..), blockIndex ) where

import Common (Solution(Solution), NoSolution(..), readNum)
import qualified Data.Set as S
import qualified Data.Map.Strict as M
import Data.Maybe (mapMaybe)
import Data.Tuple (swap)
import Data.List (sortBy)
import Data.Ord (comparing)
import Debug.Trace (traceShow)

solution = Solution "day09" run

run input = let files = parse input in (part1 files, part2 files)

type Size = Int
type Id = Int
type FreeSpace = Int
data Block = File Id Size | Space Int deriving Show

isFile :: Block -> Bool
isFile block = case block of
    (File _ _) -> True
    _ -> False

isSpace :: Block -> Bool
isSpace block = case block of
    (Space _) -> True
    _ -> False

fileSize block = case block of
    (Space _) -> 0
    (File id size) -> size

parse :: String -> [Block]
parse input = concat $ zipWith (\id (size, freeSpace) -> [File id size, Space freeSpace] ) [0 .. ] (splitBy2 input)

splitBy2 :: String -> [(Int, Int)]
splitBy2 [] = []
splitBy2 [x] = [(readNum [x], 0)]
splitBy2 (x1:x2:xs) = (readNum [x1], readNum [x2]) : splitBy2 xs

-- TODO: The solutions looks pretty ugly. They can be for sure somehow simplified.

part1 :: [Block] -> Int
part1 blocks = checksum 0 defragged where
    defragged = defragment blocks

defragment :: [Block] -> [Block]
defragment blocks = cutToSize totalFilesSize (defrag blocks (reverse blocks)) where
    totalFilesSize = sum $ map fileSize blocks

defrag :: [Block] -> [Block] -> [Block]
defrag (f@(File _ _):begin) end = f : defrag begin end
defrag (Space 0:begin) end = defrag begin end
defrag begin (Space _:end) = defrag begin end
defrag (Space spaceSize : files) (File id fileSize : filesRev)
    | spaceSize >= fileSize  = File id fileSize : defrag (Space (spaceSize - fileSize) : files) filesRev
    | otherwise              = File id spaceSize : defrag files (File id (fileSize - spaceSize) : filesRev)

cutToSize :: Size -> [Block] -> [Block]
cutToSize 0 _ = []
cutToSize n (File id size : files) = File id (min n size) : cutToSize (max 0 (n-size)) files

checksum :: Int -> [Block] -> Int
checksum _ [] = 0
checksum pos (File id size : rest) = sum (take size $ zipWith (*) [pos..] [id,id..])  +  checksum (pos + size) rest


part2 :: [Block] -> Int
part2 blocks = checksum2 (M.toList defragged) where
    blocksWithIndices = scanl blockIndex 0 blocks `zip` blocks
    spaces = S.fromList $ map (\(idx, Space size) -> (size, idx)) $ filter (isSpace . snd) blocksWithIndices  -- Set (spaceSize, idx)
    files = M.fromList $ filter (isFile . snd) blocksWithIndices  -- Map idx File
    defragged = defrag2 files spaces

blockIndex :: Int -> Block -> Int
blockIndex idx (File _ size) = idx + size
blockIndex idx (Space size) = idx + size

defrag2 :: M.Map Int Block -> S.Set (Int, Int) -> M.Map Int Block
defrag2 files spaces = fst $ foldl moveFile (files, spaces) filesToMove where
    filesToMove = reverse $ M.toList files

moveFile :: (M.Map Int Block, S.Set (Int, Int)) -> (Int, Block) -> (M.Map Int Block, S.Set (Int, Int))
moveFile (files, spaces) (fileIdx, file@(File _ fileSize)) =
    let validSpaces = sortBy (comparing snd) $ mapMaybe ((`S.lookupGE` spaces) . (,0)) [fileSize..9] in
    case validSpaces of
        [] -> (files, spaces)
        (spaceSize, spaceIdx):_ -> if spaceIdx < fileIdx then (files', spaces') else (files, spaces) where
            files' = M.insert spaceIdx file (M.delete fileIdx files)
            spaces' = if fileSize == spaceSize
                    then S.delete (spaceSize, spaceIdx) spaces
                    else S.insert (spaceSize - fileSize, spaceIdx + fileSize) (S.delete (spaceSize, spaceIdx) spaces)

checksum2 :: [(Int, Block)] -> Int
checksum2 [] = 0
checksum2 ((idx, File id size) : rest) = sum (take size $ zipWith (*) [idx..] [id,id..])  +  checksum2 rest
