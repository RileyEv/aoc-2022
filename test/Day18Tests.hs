module Day18Tests
  ( tests
  ) where

import qualified Day18                         as Day
import           Test.Tasty                     ( TestTree
                                                , testGroup
                                                )
import           Test.Tasty.HUnit               ( (@?=)
                                                , testCase
                                                )

tests :: TestTree
tests = testGroup "day18 should" [task1tests, task2tests]

task1tests :: TestTree
task1tests = testGroup
  "task1 should"
  [ testCase "example1" $ do
      input <- readFile "inputs/day18-test.txt"
      res   <- Day.task1 (map Day.reader (lines input))
      res @?= Just 64
  ]

task2tests :: TestTree
task2tests = testGroup
  "task2 should"
  [ testCase "example1" $ do
      input <- readFile "inputs/day18-test.txt"
      res   <- Day.task2 (map Day.reader (lines input))
      res @?= Nothing
  ]
