module ExamplesTest where
import Test.HUnit (test, Test(TestLabel, TestCase), assertEqual, (~:), (~=?))
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
import Control.Arrow (first)


-- cases format is as follows:
-- (part1, part2)
-- where part1 and part2 are lists: [(expected1, in1), (expected2, in2), ...]
exampleTest solution (part1Cases, part2Cases) = let
    name = solutionName solution
    makeTest part tuplePart (input, expected) = part ~: input ~: expected ~=? tuplePart (solutionRun solution input)
    in
    TestLabel name $ test [
        test $ map (makeTest "part1" fst) part1Cases,
        test $ map (makeTest "part2" snd) part2Cases ]


day01exampleInput = "3   4\n"
                 ++ "4   3\n"
                 ++ "2   5\n"
                 ++ "1   3\n"
                 ++ "3   9\n"
                 ++ "3   3\n"
day01part1 = [(day01exampleInput, 11)]
day01part2 = [(day01exampleInput, 31)]

day02exampleInput = "7 6 4 2 1\n"
                 ++ "1 2 7 8 9\n"
                 ++ "9 7 6 2 1\n"
                 ++ "1 3 2 4 5\n"
                 ++ "8 6 4 4 1\n"
                 ++ "1 3 6 7 9\n"
day02part1 = [(day02exampleInput, 2)]
day02part2 = [(day02exampleInput, 4)]

day03part1 = [("xmul(2,4)%&mul[3,7]!@^do_not_mul(5,5)+mul(32,64]then(mul(11,8)mul(8,5))", 161)]
day03part2 = [("xmul(2,4)&mul[3,7]!^don't()_mul(5,5)+mul(32,64](mul(11,8)undo()?mul(8,5))", 48)]

day04exampleInput = "MMMSXXMASM\n"
                 ++ "MSAMXMSMSA\n"
                 ++ "AMXSXMAAMM\n"
                 ++ "MSAMASMSMX\n"
                 ++ "XMASAMXAMM\n"
                 ++ "XXAMMXXAMA\n"
                 ++ "SMSMSASXSS\n"
                 ++ "SAXAMASAAA\n"
                 ++ "MAMMMXMMMM\n"
                 ++ "MXMXAXMASX\n"
day04part1 = [(day04exampleInput, 18),
            ("XMAS", 1),
            ("X\nM\nA\nS\n", 1),
            ("SAMX", 1),
            ("S\nA\nM\nX", 1),
            ("X...\n"
          ++ ".M..\n"
          ++ "..A.\n"
          ++ "...S\n", 1),
            ("S...\n"
          ++ ".A..\n"
          ++ "..M.\n"
          ++ "...X\n", 1),
            ("...X\n"
          ++ "..M.\n"
          ++ ".A..\n"
          ++ "S...\n", 1),
            ("...S\n"
          ++ "..A.\n"
          ++ ".M..\n"
          ++ "X...\n", 1)]
day04part2 = [(day04exampleInput, 9)]

day05exampleInput = "47|53\n"
                 ++ "97|13\n"
                 ++ "97|61\n"
                 ++ "97|47\n"
                 ++ "75|29\n"
                 ++ "61|13\n"
                 ++ "75|53\n"
                 ++ "29|13\n"
                 ++ "97|29\n"
                 ++ "53|29\n"
                 ++ "61|53\n"
                 ++ "97|53\n"
                 ++ "61|29\n"
                 ++ "47|13\n"
                 ++ "75|47\n"
                 ++ "97|75\n"
                 ++ "47|61\n"
                 ++ "75|61\n"
                 ++ "47|29\n"
                 ++ "75|13\n"
                 ++ "53|13\n"
                 ++ "\n"
                 ++ "75,47,61,53,29\n"
                 ++ "97,61,53,29,13\n"
                 ++ "75,29,13\n"
                 ++ "75,97,47,61,53\n"
                 ++ "61,13,29\n"
                 ++ "97,13,75,29,47\n"
day05part1 = [(day05exampleInput, 143)]
day05part2 = [(day05exampleInput, 123)]

day06exampleInput = "....#.....\n"
                 ++ ".........#\n"
                 ++ "..........\n"
                 ++ "..#.......\n"
                 ++ ".......#..\n"
                 ++ "..........\n"
                 ++ ".#..^.....\n"
                 ++ "........#.\n"
                 ++ "#.........\n"
                 ++ "......#...\n"
day06part1 = [(day06exampleInput, 41)]
day06part2 = [(day06exampleInput, 6),
              (".#.\n"
            ++ "...\n"
            ++ ".^.\n", 0)]

day07exampleInput = "190: 10 19\n"
                 ++ "3267: 81 40 27\n"
                 ++ "83: 17 5\n"
                 ++ "156: 15 6\n"
                 ++ "7290: 6 8 6 15\n"
                 ++ "161011: 16 10 13\n"
                 ++ "192: 17 8 14\n"
                 ++ "21037: 9 7 18 13\n"
                 ++ "292: 11 6 16 20\n"
day07part1 = [(day07exampleInput, 3749)]
day07part2 = [(day07exampleInput, 11387)]

day08exampleInput = "............\n"
                 ++ "........0...\n"
                 ++ ".....0......\n"
                 ++ ".......0....\n"
                 ++ "....0.......\n"
                 ++ "......A.....\n"
                 ++ "............\n"
                 ++ "............\n"
                 ++ "........A...\n"
                 ++ ".........A..\n"
                 ++ "............\n"
                 ++ "............\n"
day08part1 = [(day08exampleInput, 14)]
day08part2 = [(day08exampleInput, 34)]

day09part1 = [("2333133121414131402", 1928)]
day09part2 = []

day10part1 = []
day10part2 = []

day11part1 = []
day11part2 = []

day12part1 = [("AAAA\n"
            ++ "BBCD\n"
            ++ "BBCC\n"
            ++ "EEEC\n", 140),

              ("OOOOO\n"
            ++ "OXOXO\n"
            ++ "OOOOO\n"
            ++ "OXOXO\n"
            ++ "OOOOO\n", 772),

              ("RRRRIICCFF\n"
            ++ "RRRRIICCCF\n"
            ++ "VVRRRCCFFF\n"
            ++ "VVRCCCJFFF\n"
            ++ "VVVVCJJCFE\n"
            ++ "VVIVCCJJEE\n"
            ++ "VVIIICJJEE\n"
            ++ "MIIIIIJJEE\n"
            ++ "MIIISIJEEE\n"
            ++ "MMMISSJEEE\n", 1930)]
day12part2 = [("AAAA\n"
            ++ "BBCD\n"
            ++ "BBCC\n"
            ++ "EEEC\n", 80),

              ("EEEEE\n"
            ++ "EXXXX\n"
            ++ "EEEEE\n"
            ++ "EXXXX\n"
            ++ "EEEEE\n", 236),

              ("AAAAAA\n"
            ++ "AAABBA\n"
            ++ "AAABBA\n"
            ++ "ABBAAA\n"
            ++ "ABBAAA\n"
            ++ "AAAAAA\n", 368),

              ("RRRRIICCFF\n"
            ++ "RRRRIICCCF\n"
            ++ "VVRRRCCFFF\n"
            ++ "VVRCCCJFFF\n"
            ++ "VVVVCJJCFE\n"
            ++ "VVIVCCJJEE\n"
            ++ "VVIIICJJEE\n"
            ++ "MIIIIIJJEE\n"
            ++ "MIIISIJEEE\n"
            ++ "MMMISSJEEE\n", 1206)]

day13exampleInput = "Button A: X+94, Y+34\n"
                 ++ "Button B: X+22, Y+67\n"
                 ++ "Prize: X=8400, Y=5400\n"
                 ++ "\n"
                 ++ "Button A: X+26, Y+66\n"
                 ++ "Button B: X+67, Y+21\n"
                 ++ "Prize: X=12748, Y=12176\n"
                 ++ "\n"
                 ++ "Button A: X+17, Y+86\n"
                 ++ "Button B: X+84, Y+37\n"
                 ++ "Prize: X=7870, Y=6450\n"
                 ++ "\n"
                 ++ "Button A: X+69, Y+23\n"
                 ++ "Button B: X+27, Y+71\n"
                 ++ "Prize: X=18641, Y=10279\n"
day13part1 = [(day13exampleInput, 480)]
day13part2 = [(day13exampleInput, 875318608908)]

day14exampleInput = "11,7\n"
                 ++ "p=0,4 v=3,-3\n"
                 ++ "p=6,3 v=-1,-3\n"
                 ++ "p=10,3 v=-1,2\n"
                 ++ "p=2,0 v=2,-1\n"
                 ++ "p=0,0 v=1,3\n"
                 ++ "p=3,0 v=-2,-2\n"
                 ++ "p=7,6 v=-1,-3\n"
                 ++ "p=3,0 v=-1,-2\n"
                 ++ "p=9,3 v=2,3\n"
                 ++ "p=7,3 v=-1,2\n"
                 ++ "p=2,4 v=2,-3\n"
                 ++ "p=9,5 v=-3,-3\n"
day14part1 = [(day14exampleInput, 12)]

day14part2 = []

day15example1Input = "##########\n"
                  ++ "#..O..O.O#\n"
                  ++ "#......O.#\n"
                  ++ "#.OO..O.O#\n"
                  ++ "#..O@..O.#\n"
                  ++ "#O#..O...#\n"
                  ++ "#O..O..O.#\n"
                  ++ "#.OO.O.OO#\n"
                  ++ "#....O...#\n"
                  ++ "##########\n"
                  ++ "\n"
                  ++ "<vv>^<v^>v>^vv^v>v<>v^v<v<^vv<<<^><<><>>v<vvv<>^v^>^<<<><<v<<<v^vv^v>^\n"
                  ++ "vvv<<^>^v^^><<>>><>^<<><^vv^^<>vvv<>><^^v>^>vv<>v<<<<v<^v>^<^^>>>^<v<v\n"
                  ++ "><>vv>v^v^<>><>>>><^^>vv>v<^^^>>v^v^<^^>v^^>v^<^v>v<>>v^v^<v>v^^<^^vv<\n"
                  ++ "<<v<^>>^^^^>>>v^<>vvv^><v<<<>^^^vv^<vvv>^>v<^^^^v<>^>vvvv><>>v^<<^^^^^\n"
                  ++ "^><^><>>><>^^<<^^v>>><^<v>^<vv>>v>>>^v><>^v><<<<v>>v<v<v>vvv>^<><<>^><\n"
                  ++ "^>><>^v<><^vvv<^^<><v<<<<<><^v<<<><<<^^<v<^^^><^>>^<v^><<<^>>^v<v^v<v^\n"
                  ++ ">^>>^v>vv>^<<^v<>><<><<v<<v><>v<^vv<<<>^^v^>^^>>><<^v>>v^v><^^>>^<>vv^\n"
                  ++ "<><^^>^^^<><vvvvv^v<v<<>^v<v>v<<^><<><<><<<^^<<<^<<>><<><^^^>^^<>^>v<>\n"
                  ++ "^^>vv<^v^v<vv>^<><v<^v>^^^>>>^^vvv^>vvv<>>>^<^>>>>>^<<^v>^vvv<>^<><<v>\n"
                  ++ "v^^>>><<^^<>>^v^<v^vv<>v^<<>^<^v^v><^<<<><<^<v><v<>vv>>v><v^<vv<>v^<<^\n"
day15example2Input = "########\n"
                  ++ "#..O.O.#\n"
                  ++ "##@.O..#\n"
                  ++ "#...O..#\n"
                  ++ "#.#.O..#\n"
                  ++ "#...O..#\n"
                  ++ "#......#\n"
                  ++ "########\n"
                  ++ "\n"
                  ++ "<^^>>>vv<v>>v<<\n"
day15part1 = [(day15example1Input, 10092),
              (day15example2Input, 2028)]
day15part2 = [(day15example1Input, 9021),
              (day15example2Input, 1751)]

day16exampleInput = "###############\n"
                 ++ "#.......#....E#\n"
                 ++ "#.#.###.#.###.#\n"
                 ++ "#.....#.#...#.#\n"
                 ++ "#.###.#####.#.#\n"
                 ++ "#.#.#.......#.#\n"
                 ++ "#.#.#####.###.#\n"
                 ++ "#...........#.#\n"
                 ++ "###.#.#####.#.#\n"
                 ++ "#...#.....#.#.#\n"
                 ++ "#.#.#.###.#.#.#\n"
                 ++ "#.....#...#.#.#\n"
                 ++ "#.###.#.#.#.#.#\n"
                 ++ "#S..#.....#...#\n"
                 ++ "###############\n"
day16part1 = [{- (day16exampleInput, 7036) -}]
day16part2 = []

day17part1 = []
day17part2 = []

day18part1 = []
day18part2 = []

day19part1 = []
day19part2 = []

day20part1 = []
day20part2 = []

day21part1 = []
day21part2 = []

day22part1 = []
day22part2 = []

day23part1 = []
day23part2 = []

day24part1 = []
day24part2 = []

day25part1 = []
day25part2 = []

tests = TestLabel "ExamplesTest" $ test
    [ exampleTest Day01.solution (day01part1, day01part2)
    , exampleTest Day02.solution (day02part1, day02part2)
    , exampleTest Day03.solution (day03part1, day03part2)
    , exampleTest Day04.solution (day04part1, day04part2)
    , exampleTest Day05.solution (day05part1, day05part2)
    , exampleTest Day06.solution (day06part1, day06part2)
    , exampleTest Day07.solution (day07part1, day07part2)
    , exampleTest Day08.solution (day08part1, day08part2)
    , exampleTest Day09.solution (day09part1, day09part2)
    , exampleTest Day10.solution (day10part1, day10part2)
    , exampleTest Day11.solution (day11part1, day11part2)
    , exampleTest Day12.solution (day12part1, day12part2)
    , exampleTest Day13.solution (day13part1, day13part2)
    , exampleTest Day14.solution (day14part1, day14part2)
    , exampleTest Day15.solution (day15part1, day15part2)
    , exampleTest Day16.solution (day16part1, day16part2)
    , exampleTest Day17.solution (day17part1, day17part2)
    , exampleTest Day18.solution (day18part1, day18part2)
    , exampleTest Day19.solution (day19part1, day19part2)
    , exampleTest Day20.solution (day20part1, day20part2)
    , exampleTest Day21.solution (day21part1, day21part2)
    , exampleTest Day22.solution (day22part1, day22part2)
    , exampleTest Day23.solution (day23part1, day23part2)
    , exampleTest Day24.solution (day24part1, day24part2)
    , exampleTest Day25.solution (day25part1, day25part2) ]
