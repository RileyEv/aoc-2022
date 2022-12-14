module Day11Tests
  ( tests
  ) where

import qualified Day11                         as Day
import           Test.Tasty                     ( TestTree
                                                , testGroup
                                                )
import           Test.Tasty.HUnit               ( (@?=)
                                                , testCase
                                                )

tests :: TestTree
tests = testGroup "day11 should" [task1tests, task2tests]

task1tests :: TestTree
task1tests = testGroup
  "task1 should"
  [ testCase "example1" $ do
      res <- Day.task1
        [ Day.Monkey [79, 98] (* 19) 23 2 3 0 0
        , Day.Monkey [54, 65, 75, 74] (+ 6) 19 2 0 0 1
        , Day.Monkey [79, 60, 97] (\x -> x * x) 13 1 3 0 2
        , Day.Monkey [74] (+ 3) 17 0 1 0 3
        ]
      res @?= Just 10605
  ]

task2tests :: TestTree
task2tests = testGroup
  "task2 should"
  [ testCase "example1" $ do
      res <- Day.task2
        [ Day.Monkey [79, 98] (* 19) 23 2 3 0 0
        , Day.Monkey [54, 65, 75, 74] (+ 6) 19 2 0 0 1
        , Day.Monkey [79, 60, 97] (\x -> x * x) 13 1 3 0 2
        , Day.Monkey [74] (+ 3) 17 0 1 0 3
        ]
      res @?= Just 2713310158
  ]
