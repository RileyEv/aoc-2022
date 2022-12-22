module Day15Tests
  ( tests
  ) where

import qualified Day15                         as Day
import           Test.Tasty                     ( TestTree
                                                , testGroup
                                                )
import           Test.Tasty.HUnit               ( (@?=)
                                                , testCase
                                                )

tests :: TestTree
tests = testGroup "day15 should" [task1tests, task2tests]

task1tests :: TestTree
task1tests = testGroup
  "task1 should"
  [ testCase "example1" $ do
      input <- readFile "inputs/day15-test.txt"
      res   <- Day.task1 10 (Day.reader input)
      res @?= Just 26
  ]

task2tests :: TestTree
task2tests = testGroup
  "task2 should"
  [ testCase "example1" $ do
      input <- readFile "inputs/day15-test.txt"
      res   <- Day.task2 (20, 20) (Day.reader input)
      res @?= Just 56000011
  ]
