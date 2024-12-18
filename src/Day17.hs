module Day17 ( solution ) where

import Common (Solution(Solution), NoSolution(..), readNum, parseComaSeparatedNums, inArrayBounds)
import Data.List.Split (splitOn)
import Data.List (intercalate)
import Data.Bifunctor (first)
import Data.Array (Array, listArray, (!), bounds, elems)
import Data.Bits (xor)

solution = Solution "day17" run

run input = let (registers, instructinos) = parse input in (part1 registers instructinos, NoSolution)

data Registers = Registers Int Int Int
type Program = Array Int Int

parse :: String -> (Registers, Program)
parse input = (Registers a b c, program) where
    [registersStr, instructionsStr] = splitOn "\n\n" input
    [a, b, c] = map parseRegister $ splitOn "\n" registersStr
    instructions = parseComaSeparatedNums (last $ splitOn ": " instructionsStr)
    program = listArray (0, length instructions - 1) instructions

parseRegister :: String -> Int
parseRegister str = readNum s where
    [_, s] = splitOn ": " str


part1 :: Registers -> Program  -> String
part1 registers program = intercalate "," $ map show $ reverse $ runProgram program registers

-- of course simple brute force is waaaaaay to slow
-- part2 :: Registers -> Program  -> Int
-- part2 (Registers _ b c) program = fst validRun where
--     runs = map (\x -> (x, runProgram program (Registers x b c))) [117400..]
--     programList = reverse $ elems program
--     validRun = head $ dropWhile ((/= programList) . snd) runs

runProgram :: Program -> Registers -> [Int]
runProgram program registers = out where
    iterations = iterate (makeInstruction program) (registers, [], 0)
    (_, maxPointer) = bounds program
    validIterations = takeWhile (\(_, _, pointer) -> pointer + 1 <= maxPointer) iterations
    (_, out, _) = last validIterations

makeInstruction :: Program -> (Registers, [Int], Int) -> (Registers, [Int], Int)
makeInstruction program state@(registers@(Registers a b c), out, pointer) =
    let instruction = program ! pointer
        x = program ! (pointer + 1)
        op = getOp registers x
    in case instruction of
        0 -> (Registers (a `div` (2^op)) b c, out, pointer + 2)
        1 -> (Registers a (b `xor` x) c, out, pointer + 2)
        2 -> (Registers a (op `mod` 8) c, out, pointer + 2)
        3 -> if a == 0 then (Registers a b c, out, pointer + 2) else (Registers a b c, out, x)
        4 -> (Registers a (b `xor` c) c, out, pointer + 2)
        5 -> (Registers a b c, (op `mod` 8):out, pointer + 2)
        6 -> (Registers a (a `div` (2^op)) c, out, pointer + 2)
        7 -> (Registers a b (a `div` (2^op)), out, pointer + 2)

getOp :: Registers -> Int -> Int
getOp (Registers a b c) x = case x of
    0 -> 0
    1 -> 1
    2 -> 2
    3 -> 3
    4 -> a
    5 -> b
    6 -> c
    7 -> undefined
