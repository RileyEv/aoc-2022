module Day10Tests
  ( tests
  ) where

import qualified Day10                         as Day
import           Test.Tasty                     ( TestTree
                                                , testGroup
                                                )
import           Test.Tasty.HUnit               ( (@?=)
                                                , testCase
                                                )

tests :: TestTree
tests = testGroup "day10 should" [task1tests, task2tests]

task1tests :: TestTree
task1tests = testGroup
  "task1 should"
  [ testCase "example1" $ do
      input <- readFile "inputs/day10-test.txt"
      res   <- Day.task1 (Day.reader input)
      res @?= Just 13140
  ]

task2tests :: TestTree
task2tests = testGroup
  "task2 should"
  [ testCase "example1" $ do
      input <- readFile "inputs/day10-test.txt"
      res   <- Day.task2 (Day.reader input)
      res @?= Nothing
  ]
