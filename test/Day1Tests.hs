module Day1Tests
    ( tests
    ) where

import qualified Day1                          as Day
import           Test.Tasty                     ( TestTree
                                                , testGroup
                                                )
import           Test.Tasty.HUnit               ( (@?=)
                                                , testCase
                                                )

tests :: TestTree
tests = testGroup "day 1 should" [task1tests, task2tests]

task1tests :: TestTree
task1tests = testGroup
    "task1 should"
    [ testCase "example1" $ do
          res <- Day.task1
              [ Just 1000
              , Just 2000
              , Just 3000
              , Nothing
              , Just 4000
              , Nothing
              , Just 5000
              , Just 6000
              , Nothing
              , Just 7000
              , Just 8000
              , Just 9000
              , Nothing
              , Just 10000
              ]
          res @?= Just 24000
    ]

task2tests :: TestTree
task2tests = testGroup
    "task2 should"
    [ testCase "example1" $ do
          res <- Day.task2
              [ Just 1000
              , Just 2000
              , Just 3000
              , Nothing
              , Just 4000
              , Nothing
              , Just 5000
              , Just 6000
              , Nothing
              , Just 7000
              , Just 8000
              , Just 9000
              , Nothing
              , Just 10000
              ]
          res @?= Just 45000
    ]
