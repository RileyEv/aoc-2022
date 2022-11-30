module Main
    ( main
    ) where

import           Common                         ( Task(..) )
import qualified Day1                           ( run )
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
            Just 2  -> error "Day not setup yet"
            Just 3  -> error "Day not setup yet"
            Just 4  -> error "Day not setup yet"
            Just 5  -> error "Day not setup yet"
            Just 6  -> error "Day not setup yet"
            Just 7  -> error "Day not setup yet"
            Just 8  -> error "Day not setup yet"
            Just 9  -> error "Day not setup yet"
            Just 10 -> error "Day not setup yet"
            Just 11 -> error "Day not setup yet"
            Just 12 -> error "Day not setup yet"
            Just 13 -> error "Day not setup yet"
            Just 14 -> error "Day not setup yet"
            Just 15 -> error "Day not setup yet"
            Just 16 -> error "Day not setup yet"
            Just 17 -> error "Day not setup yet"
            Just 18 -> error "Day not setup yet"
            Just 19 -> error "Day not setup yet"
            Just 20 -> error "Day not setup yet"
            Just 21 -> error "Day not setup yet"
            Just 22 -> error "Day not setup yet"
            Just 23 -> error "Day not setup yet"
            Just 24 -> error "Day not setup yet"
            Just _  -> error "there's only 24 days!"
            Nothing -> error "unable to parse day"

    dayTask selectedTask
