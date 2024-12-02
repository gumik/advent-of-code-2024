module Common (
    Solution(..),
    NoSolution(..),
    listOfNumbers,
    (!?),
    readNum,
    readNum',
    parseComaSeparatedNums,
    toDecimal,
    parseArray,
    showArray,
    inArrayBounds,
    ShowString(..),
    toTuple,
    toTriple) where
import Numeric (readInt)
import Data.List.Split (splitOn, chunksOf)
import Data.Array

data Solution a b = Solution {
    solutionName :: String,
    solutionRun :: String -> (a, b)
}

data NoSolution = NoSolution deriving (Eq)
instance Show NoSolution where
    show NoSolution = "(no solution)"

newtype ShowString = ShowString String
instance Show ShowString where
    show (ShowString s) = s

listOfNumbers :: String -> [Int]
listOfNumbers content = map read (lines content) :: [Int]

(!?) :: [a] -> Int -> Maybe a
xs !? n = if n >= 0 && n < length xs
    then Just $ xs !! n
    else Nothing

readNum :: String -> Int
readNum = read

readNum' :: Char -> Int
readNum' c = readNum [c]

parseComaSeparatedNums :: String -> [Int]
parseComaSeparatedNums = map readNum . splitOn ","

toDecimal :: Int -> [Int] -> Int
toDecimal nary digits = sum $ zipWith (\d c -> d * nary^c) (reverse digits) [0..]

parseArray :: (Char -> a) -> String -> Array (Int, Int) a
parseArray readChar input = let
    parsedLines = map (map readChar) $ lines input
    width = length $ head parsedLines
    height = length parsedLines
    in listArray ((0, 0), (height-1, width-1)) $ concat parsedLines

showArray :: (Show a) => Array (Int, Int) a -> String
showArray arr = let
    ((_, w1), (_, w2)) = bounds arr
    width = w2 - w1 + 1
    in unlines $ chunksOf width $ concatMap show $ elems arr

inArrayBounds arr (y, x) = let
    ((h0, w0), (hm, wm)) = bounds arr
    in x >= w0 && y >= h0 && x <= wm && y <= hm

toTuple :: [a] -> (a, a)
toTuple [x, y] = (x, y)
toTuple _ = error "toTuple got list with length /= than 2"

toTriple :: [a] -> (a, a, a)
toTriple [x, y, z] = (x, y, z)
toTriple _ = error "toTuple got list with length /= than 3"
