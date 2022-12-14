module Day9Tests
  ( tests
  ) where

import qualified Day9                          as Day
import           Test.Tasty                     ( TestTree
                                                , testGroup
                                                )
import           Test.Tasty.HUnit               ( (@?=)
                                                , testCase
                                                )

tests :: TestTree
tests = testGroup "day9 should" [task2tests]

task1tests :: TestTree
task1tests = testGroup
  "task1 should"
  [ testCase "example1" $ do
      testInput <- lines <$> readFile "inputs/day9-test.txt"
      res       <- Day.task1 (map Day.reader testInput)
      res @?= Just 13
  ]

task2tests :: TestTree
task2tests = testGroup
  "task2 should"
  -- [ testCase "example1" $ do
  --   testInput <- lines <$> readFile "inputs/day9-test.txt"
  --   res       <- Day.task2 (map Day.reader testInput)
  --   res @?= Just 1
  [ testCase "example2" $ do
      testInput <- lines <$> readFile "inputs/day9-test2.txt"
      res       <- Day.task2 (map Day.reader testInput)
      res @?= Just 36
  ]
