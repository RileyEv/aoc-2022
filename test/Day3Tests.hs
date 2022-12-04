module Day3Tests
  ( tests
  ) where

import qualified Day3                          as Day
import           Test.Tasty                     ( TestTree
                                                , testGroup
                                                )
import           Test.Tasty.HUnit               ( (@?=)
                                                , testCase
                                                )

tests :: TestTree
tests = testGroup "day3 should" [task1tests, task2tests]

task1tests :: TestTree
task1tests = testGroup
  "task1 should"
  [ testCase "example1" $ do
      res <- Day.task1
        [ "vJrwpWtwJgWrhcsFMMfFFhFp"
        , "jqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL"
        , "PmmdzqPrVvPwwTWBwg"
        , "wMqvLMZHhHMvwLHjbvcjnnSBnvTQFn"
        , "ttgJtRGJQctTZtZT"
        , "CrZsJsPPZsGzwwsLwLmpwMDw"
        ]
      res @?= Just 157
  ]

task2tests :: TestTree
task2tests = testGroup
  "task2 should"
  [ testCase "example1" $ do
      res <- Day.task2
        [ "vJrwpWtwJgWrhcsFMMfFFhFp"
        , "jqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL"
        , "PmmdzqPrVvPwwTWBwg"
        , "wMqvLMZHhHMvwLHjbvcjnnSBnvTQFn"
        , "ttgJtRGJQctTZtZT"
        , "CrZsJsPPZsGzwwsLwLmpwMDw"
        ]
      res @?= Just 70
  ]
