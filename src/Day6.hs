module Day6
    ( run
    , task1
    , task2
    ) where
import           Common                         ( inputFileName
                                                , runTasksLines
                                                )

task1 :: [Int] -> IO (Maybe Int)
task1 _ = return Nothing

task2 :: [Int] -> IO (Maybe Int)
task2 _ = return Nothing

-- Does not need to be read, this is just some way to manipulate each line of the input file
run = runTasksLines (inputFileName 'run) read task1 task2
