module Main
    ( main
    ) where

import           Common                         ( Task(..) )
import qualified Day1                           ( run )
import qualified Day10                          ( run )
import qualified Day11                          ( run )
import qualified Day12                          ( run )
import qualified Day13                          ( run )
import qualified Day14                          ( run )
import qualified Day15                          ( run )
import qualified Day19                          ( run )
import qualified Day2                           ( run )
import qualified Day20                          ( run )
import qualified Day3                           ( run )
import qualified Day4                           ( run )
import qualified Day5                           ( run )
import qualified Day6                           ( run )
import qualified Day7                           ( run )
import qualified Day8                           ( run )
import qualified Day9                           ( run )
import           System.Environment             ( getArgs )
import           Text.Read                      ( readMaybe )


main :: IO ()
main = do
    (strDay : possibleTask) <- getArgs
    let day          = readMaybe strDay :: Maybe Int
        selectedTask = case possibleTask of
            []              -> All
            ("1" : _)       -> Task1
            ("2" : _)       -> Task2
            _everythingElse -> error "invalid task option"

    let dayTask = case day of
            Just 1  -> Day1.run
            Just 2  -> Day2.run
            Just 3  -> Day3.run
            Just 4  -> Day4.run
            Just 5  -> Day5.run
            Just 6  -> Day6.run
            Just 7  -> Day7.run
            Just 8  -> Day8.run
            Just 9  -> Day9.run
            Just 10 -> Day10.run
            Just 11 -> Day11.run
            Just 12 -> Day12.run
            Just 13 -> Day13.run
            Just 14 -> Day14.run
            Just 15 -> Day15.run
            Just 16 -> error "Day not setup yet"
            Just 17 -> error "Day not setup yet"
            Just 18 -> error "Day not setup yet"
            Just 19 -> Day19.run
            Just 20 -> Day20.run
            Just 21 -> error "Day not setup yet"
            Just 22 -> error "Day not setup yet"
            Just 23 -> error "Day not setup yet"
            Just 24 -> error "Day not setup yet"
            Just _  -> error "there's only 24 days!"
            Nothing -> error "unable to parse day"

    dayTask selectedTask
