module Day20Tests
  ( tests
  ) where

import qualified Day20                         as Day
import           Test.Tasty                     ( TestTree
                                                , testGroup
                                                )
import           Test.Tasty.HUnit               ( (@?=)
                                                , testCase
                                                )

tests :: TestTree
tests = testGroup "day20 should" [task1tests, task2tests]

task1tests :: TestTree
task1tests = testGroup
  "task1 should"
  [ testCase "example1" $ do
      input <- readFile "inputs/day20-test.txt"
      res   <- Day.task1 (map Day.reader (lines input))
      res @?= Just 3
  ]

task2tests :: TestTree
task2tests = testGroup
  "task2 should"
  [ testCase "example1" $ do
      input <- readFile "inputs/day20-test.txt"
      res   <- Day.task2 (map Day.reader (lines input))
      res @?= Just 1623178306
  ]
