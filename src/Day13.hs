module Day13 ( solution ) where

import Common (Solution(Solution), NoSolution(..), readNum)
import Data.List.Split (splitOn)
import Data.Char (isNumber)
import Data.Maybe (mapMaybe)
import Debug.Trace (traceShow)

solution = Solution "day13" run

run input = let machines = parse input in (part1 machines, part2 machines)

type Button = (Int, Int)
type Prize = (Int, Int)
data ClawMachine = ClawMachine Button Button Prize deriving Show

parse :: String -> [ClawMachine]
parse = map parseMachine . splitOn "\n\n"

parseMachine :: String -> ClawMachine
parseMachine input = ClawMachine (buttonAX, buttonAY) (buttonBX, buttonBY) (prizeX, prizeY) where
    (buttonAstr : buttonBstr : prizeStr : _) = splitOn "\n" input
    buttonAX = readCoord "X+" buttonAstr
    buttonAY = readCoord "Y+" buttonAstr
    buttonBX = readCoord "X+" buttonBstr
    buttonBY = readCoord "Y+" buttonBstr
    prizeX = readCoord "X=" prizeStr
    prizeY = readCoord "Y=" prizeStr

readCoord :: String -> String -> Int
readCoord c = readNum . takeWhile isNumber . (!! 1) . splitOn c

{-
The solution must satisfy the below system of equations:

n*x1 + m*x2 = x
n*y1 + m*y2 = y

where
    (x1, y1) - button A move
    (x2, y2) - button B move
    (x, y) - prize location
    n, m - numbers to find

The system has up to one integer solution when x1 /= 0, x2 /= 0 and y1/x1 /= y2/x2 (vectors are not collinear)
Fortunately all the claw machines met the above conditions so if a solution is found
it is therefore the optimal one.
-}
part1 :: [ClawMachine] -> Int
part1 = sum . mapMaybe leastTokens

part2 :: [ClawMachine] -> Int
part2 = sum . mapMaybe (leastTokens . correctPrize) where
    correctPrize (ClawMachine a b (x, y)) = ClawMachine a b (x + 10000000000000, y + 10000000000000)

leastTokens :: ClawMachine -> Maybe Int
leastTokens clawMachine = do
        (n, m) <- numberOfPresses clawMachine
        return $ 3*n + m

numberOfPresses :: ClawMachine -> Maybe (Int, Int)
numberOfPresses (ClawMachine (x1, y1) (x2, y2) (x, y)) = if mRest /= 0 || nRest /= 0
        then Nothing
        else Just (n, m)
      where
    (m, mRest) = (x1 * y - x * y1) `divMod` (x1 * y2 - x2 * y1)
    (n, nRest) = (x - m * x2) `divMod` x1
