module InputTest where
import Test.HUnit (test, Test(TestLabel, TestCase), assertEqual)
import Common (Solution(solutionRun, solutionName), NoSolution(..))
import qualified Day01
import qualified Day02
import qualified Day03
import qualified Day04
import qualified Day05
import qualified Day06
import qualified Day07
import qualified Day08
import qualified Day09
import qualified Day10
import qualified Day11
import qualified Day12
import qualified Day13
import qualified Day14
import qualified Day15
import qualified Day16
import qualified Day17
import qualified Day18
import qualified Day19
import qualified Day20
import qualified Day21
import qualified Day22
import qualified Day23
import qualified Day24
import qualified Day25


inputTest solution expected = let name = solutionName solution in
    TestLabel name $ TestCase (do input <- readFile $ "data/" ++ name ++ "-input.txt"
                                  assertEqual "" expected (solutionRun solution input))

tests = TestLabel "InputTest" $ test
    [ inputTest Day01.solution (2970687, 23963899)
    , inputTest Day02.solution (379, 430)
    , inputTest Day03.solution (175615763, 74361272)
    , inputTest Day04.solution (2545, 1886)
    , inputTest Day05.solution (5762, 4130)
    -- , inputTest Day06.solution (4559, 1604)  -- long running
    , inputTest Day07.solution (7710205485870, 20928985450275)
    , inputTest Day08.solution (244, 912)
    , inputTest Day09.solution (6154342787400, NoSolution)
    , inputTest Day10.solution (NoSolution, NoSolution)
    , inputTest Day11.solution (NoSolution, NoSolution)
    , inputTest Day12.solution (1396298, 853588)
    , inputTest Day13.solution (NoSolution, NoSolution)
    , inputTest Day14.solution (NoSolution, NoSolution)
    , inputTest Day15.solution (NoSolution, NoSolution)
    , inputTest Day16.solution (NoSolution, NoSolution)
    , inputTest Day17.solution (NoSolution, NoSolution)
    , inputTest Day18.solution (NoSolution, NoSolution)
    , inputTest Day19.solution (NoSolution, NoSolution)
    , inputTest Day20.solution (NoSolution, NoSolution)
    , inputTest Day21.solution (NoSolution, NoSolution)
    , inputTest Day22.solution (NoSolution, NoSolution)
    , inputTest Day23.solution (NoSolution, NoSolution)
    , inputTest Day24.solution (NoSolution, NoSolution)
    , inputTest Day25.solution (NoSolution, NoSolution)
    ]
