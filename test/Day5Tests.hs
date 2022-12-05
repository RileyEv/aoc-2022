module Day5Tests
  ( tests
  ) where

import           Data.Vector                       ( fromList )
import qualified Day5                          as Day
import           Test.Tasty                     ( TestTree
                                                , testGroup
                                                )
import           Test.Tasty.HUnit               ( (@?=)
                                                , testCase
                                                )

tests :: TestTree
tests = testGroup "day5 should" [task1tests, task2tests]

task1tests :: TestTree
task1tests = testGroup
  "task1 should"
  [ testCase "example1" $ do
      res <- Day.task1
        (Day.Mission
          (fromList
            ["NZ", "DCM", "P"]
          )
          [ Day.Move 1 2 1
          , Day.Move 3 1 3
          , Day.Move 2 2 1
          , Day.Move 1 1 2
          ]
        )
      res @?= Just "CMZ"
  ]

task2tests :: TestTree
task2tests = testGroup
  "task2 should"
  [ testCase "example1" $ do
      res <- Day.task2
        (Day.Mission
          (fromList
            ["NZ", "DCM", "P"]
          )
          [ Day.Move 1 2 1
          , Day.Move 3 1 3
          , Day.Move 2 2 1
          , Day.Move 1 1 2
          ]
        )
      res @?= Just "MCD"
  ]
