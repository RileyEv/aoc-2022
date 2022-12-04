module Day4Tests
  ( tests
  ) where

import qualified Day4                          as Day
import           Test.Tasty                     ( TestTree
                                                , testGroup
                                                )
import           Test.Tasty.HUnit               ( (@?=)
                                                , testCase
                                                )

tests :: TestTree
tests = testGroup "day4 should" [task1tests, task2tests, task3tests]

task1tests :: TestTree
task1tests = testGroup
  "task1 should"
  [ testCase "example1" $ do
      res <- Day.task1
        [ Day.ElfPair (Day.ElfWork 2 4) (Day.ElfWork 6 8)
        , Day.ElfPair (Day.ElfWork 2 3) (Day.ElfWork 4 5)
        , Day.ElfPair (Day.ElfWork 5 7) (Day.ElfWork 7 9)
        , Day.ElfPair (Day.ElfWork 2 8) (Day.ElfWork 3 7)
        , Day.ElfPair (Day.ElfWork 6 6) (Day.ElfWork 4 6)
        , Day.ElfPair (Day.ElfWork 2 6) (Day.ElfWork 4 8)
        ]
      res @?= Just 2
  ]

task2tests :: TestTree
task2tests = testGroup
  "task2 should"
  [ testCase "example1" $ do
      res <- Day.task2
        [ Day.ElfPair (Day.ElfWork 2 4) (Day.ElfWork 6 8)
        , Day.ElfPair (Day.ElfWork 2 3) (Day.ElfWork 4 5)
        , Day.ElfPair (Day.ElfWork 5 7) (Day.ElfWork 7 9)
        , Day.ElfPair (Day.ElfWork 2 8) (Day.ElfWork 3 7)
        , Day.ElfPair (Day.ElfWork 6 6) (Day.ElfWork 4 6)
        , Day.ElfPair (Day.ElfWork 2 6) (Day.ElfWork 4 8)
        ]
      res @?= Just 4
  ]

task3tests :: TestTree
task3tests = testGroup
  "task3 should"
  [ testCase "example1" $ do
      res <- Day.task3
        [ Day.ElfPair (Day.ElfWork 2 4) (Day.ElfWork 6 8)
        , Day.ElfPair (Day.ElfWork 2 3) (Day.ElfWork 4 5)
        , Day.ElfPair (Day.ElfWork 5 7) (Day.ElfWork 7 9)
        , Day.ElfPair (Day.ElfWork 2 8) (Day.ElfWork 3 7)
        , Day.ElfPair (Day.ElfWork 6 6) (Day.ElfWork 4 6)
        , Day.ElfPair (Day.ElfWork 2 6) (Day.ElfWork 4 8)
        ]
      res @?= Just 11
  ]
