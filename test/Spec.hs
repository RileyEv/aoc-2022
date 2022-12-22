import           Test.Tasty                     ( TestTree
                                                , defaultMain
                                                , testGroup
                                                )

import qualified Day10Tests
import qualified Day11Tests
import qualified Day12Tests
import qualified Day13Tests
import qualified Day14Tests
import qualified Day15Tests
import qualified Day18Tests
import qualified Day19Tests
import qualified Day1Tests
import qualified Day20Tests
import qualified Day2Tests
import qualified Day3Tests
import qualified Day4Tests
import qualified Day5Tests
import qualified Day6Tests
import qualified Day7Tests
import qualified Day8Tests
import qualified Day9Tests
import qualified TemplateTests


main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup
      "Tests"
      [ TemplateTests.tests
      , Day1Tests.tests
      , Day2Tests.tests
      , Day3Tests.tests
      , Day4Tests.tests
      , Day5Tests.tests
      , Day6Tests.tests
      , Day7Tests.tests
      , Day8Tests.tests
      , Day9Tests.tests
      , Day10Tests.tests
      , Day11Tests.tests
      , Day12Tests.tests
      , Day13Tests.tests
      , Day14Tests.tests
      , Day15Tests.tests
      , Day18Tests.tests
      , Day19Tests.tests
      , Day20Tests.tests
      ]
