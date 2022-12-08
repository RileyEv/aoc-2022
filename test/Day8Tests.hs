module Day8Tests
  ( tests
  ) where

import qualified Day8                          as Day
import           Test.Tasty                     ( TestTree
                                                , testGroup
                                                )
import           Test.Tasty.HUnit               ( (@?=)
                                                , testCase
                                                )

tests :: TestTree
tests = testGroup "day8 should" [task1tests, task2tests]

task1tests :: TestTree
task1tests = testGroup
  "task1 should"
  [ testCase "example1" $ do
      res <- Day.task1 (Day.reader "30373\n25512\n65332\n33549\n35390")
      res @?= Just 21
  ]

task2tests :: TestTree
task2tests = testGroup
  "task2 should"
  -- [ testCase "example1" $ do
  --   res <- Day.task2 (Day.reader "30373\n25512\n65332\n33549\n35390")
  --   res @?= Just 8
  [ testCase "mini" $ do
      input <- readFile "inputs/day8-mini.txt"
      res   <- Day.task2 (Day.reader input)
      res @?= Just 2700
  ]
