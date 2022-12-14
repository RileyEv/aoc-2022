module Day12Tests
  ( tests
  ) where

import qualified Day12                         as Day
import           Test.Tasty                     ( TestTree
                                                , testGroup
                                                )
import           Test.Tasty.HUnit               ( (@?=)
                                                , testCase
                                                )

tests :: TestTree
tests = testGroup "day12 should" [task1tests, task2tests]

task1tests :: TestTree
task1tests = testGroup
  "task1 should"
  [ testCase "example1" $ do
      input <- readFile "inputs/day12-test.txt"
      res   <- Day.task1 (Day.reader input)
      res @?= Nothing
  ]

task2tests :: TestTree
task2tests = testGroup
  "task2 should"
  [ testCase "example1" $ do
      input <- readFile "inputs/day12-test.txt"
      res   <- Day.task2 (Day.reader input)
      res @?= Nothing
  ]
