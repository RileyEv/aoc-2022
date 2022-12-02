module Day2Tests
  ( tests
  ) where

import qualified Day2                          as Day
import           Test.Tasty                     ( TestTree
                                                , testGroup
                                                )
import           Test.Tasty.HUnit               ( (@?=)
                                                , testCase
                                                )

tests :: TestTree
tests = testGroup "day2 should" [task1tests, task2tests]

task1tests :: TestTree
task1tests = testGroup
  "task1 should"
  [ testCase "example1" $ do
      res <- Day.task1
        [ (Day.A, Day.Y)
        , (Day.B, Day.X)
        , (Day.C, Day.Z)
        ]
      res @?= Just 15
  ]

task2tests :: TestTree
task2tests = testGroup
  "task2 should"
  [ testCase "example1" $ do
      res <- Day.task2
        [ (Day.A, Day.Y)
        , (Day.B, Day.X)
        , (Day.C, Day.Z)
        ]

      res @?= Just 12
  ]
