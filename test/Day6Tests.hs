module Day6Tests
  ( tests
  ) where

import qualified Day6                          as Day
import           Test.Tasty                     ( TestTree
                                                , testGroup
                                                )
import           Test.Tasty.HUnit               ( (@?=)
                                                , testCase
                                                )

tests :: TestTree
tests = testGroup "day6 should" [task1tests, task2tests]

task1tests :: TestTree
task1tests = testGroup
  "task1 should"
  [ testCase "example1" $ do
    res <- Day.task1 "bvwbjplbgvbhsrlpgdmjqwftvncz"
    res @?= Just 5
  , testCase "example2" $ do
    res <- Day.task1 "nppdvjthqldpwncqszvftbrmjlhg"
    res @?= Just 6
  , testCase "example3" $ do
    res <- Day.task1 "nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg"
    res @?= Just 10
  , testCase "example4" $ do
    res <- Day.task1 "zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw"
    res @?= Just 11
  , testCase "example walkthrough" $ do
    res <- Day.task1 "mjqjpqmgbljsphdztnvjfqwrcgsmlb"
    res @?= Just 7
  ]

task2tests :: TestTree
task2tests = testGroup
  "task2 should"
  [ testCase "example1" $ do
    res <- Day.task2 "bvwbjplbgvbhsrlpgdmjqwftvncz"
    res @?= Just 23
  , testCase "example2" $ do
    res <- Day.task2 "nppdvjthqldpwncqszvftbrmjlhg"
    res @?= Just 23
  , testCase "example3" $ do
    res <- Day.task2 "nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg"
    res @?= Just 29
  , testCase "example4" $ do
    res <- Day.task2 "zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw"
    res @?= Just 26
  , testCase "example walkthrough" $ do
    res <- Day.task2 "mjqjpqmgbljsphdztnvjfqwrcgsmlb"
    res @?= Just 19
  ]
