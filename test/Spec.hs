import Test.HUnit (runTestTT, Test(TestList, TestLabel, TestCase))
import qualified ExamplesTest (tests)
import qualified InputTest (tests)
import System.Environment (getArgs)
import Data.Bits (Bits(xor))
import Debug.Trace (trace)

main = do
    args <- getArgs
    let filter = case args of
                    [] -> ""
                    _  -> head args
    runTestTT $ TestList (map (filterTestList filter) [ ExamplesTest.tests, InputTest.tests ])

filterTestList :: String -> Test -> Test
filterTestList "" test = test
filterTestList name test = case test of
    TestLabel l t  -> TestLabel l (filterTestList name t)
    TestList tests -> TestList $ filter filterTest tests where
        filterTest (TestLabel label _) = name == label
