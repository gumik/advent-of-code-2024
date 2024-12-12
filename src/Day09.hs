module Day09 ( solution, defrag, cutToSize, Block(..) ) where

import Common (Solution(Solution), NoSolution(..), readNum)

solution = Solution "day09" run

run input = let files = parse input in (part1 files, NoSolution)

type Size = Int
type Id = Int
type FreeSpace = Int
data Block = File Id Size | Space Int deriving Show

fileSize block = case block of
    (Space _) -> 0
    (File id size) -> size

parse :: String -> [Block]
parse input = concat $ zipWith (\id (size, freeSpace) -> [File id size, Space freeSpace] ) [0 .. ] (splitBy2 input)

splitBy2 :: String -> [(Int, Int)]
splitBy2 [] = []
splitBy2 [x] = [(readNum [x], 0)]
splitBy2 (x1:x2:xs) = (readNum [x1], readNum [x2]) : splitBy2 xs

part1 :: [Block] -> Int
part1 blocks = checksum 0 defragged where
    defragged = defragment blocks

--
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

-- part2 idea
-- empty spaces map:
--   Map spaceSize (Set position)
-- positions map:
--   Map position Block
-- eg. 111..22...333..444
-- empty spaces: [(2, [3, 13]), (3, [7]))
-- positions: [(0, File 1 3), (3, Space 2), (5, File 2 2), ...]
