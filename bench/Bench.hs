import qualified Day1                           ( reader
                                                , task1
                                                , task2
                                                )
import qualified Day19                          ( reader
                                                , task1
                                                , task2
                                                )
import qualified Day2                           ( reader
                                                , task1
                                                , task2
                                                )
import qualified Day20                          ( reader
                                                , task1
                                                , task2
                                                )
import qualified Day3                           ( reader
                                                , task1
                                                , task2
                                                )
import qualified Day4                           ( reader
                                                , task1
                                                , task2
                                                )
import qualified Day5                           ( reader
                                                , task1
                                                , task2
                                                )
import qualified Day6                           ( reader
                                                , task1
                                                , task2
                                                )
import qualified Day7                           ( reader
                                                , task1
                                                , task2
                                                )
import qualified Day8                           ( reader
                                                , task1
                                                , task2
                                                )
import qualified Day9                           ( reader
                                                , task1
                                                , task2
                                                )
import qualified Template                       ( reader
                                                , task1
                                                , task2
                                                )

import           Common                         ( Task(..)
                                                , inputFile
                                                , inputFileLinesMap
                                                )

import           Criterion.Main

runTaskLines
    :: (Show b)
    => FilePath
    -> (String -> a)
    -> ([a] -> IO (Maybe b))
    -> IO (Maybe b)
runTaskLines fname mapFunc t = inputFileLinesMap mapFunc fname >>= t

runTask
    :: (Show b)
    => FilePath
    -> (String -> a)
    -> (a -> IO (Maybe b))
    -> IO (Maybe b)
runTask fname mapFunc t = inputFile fname >>= t . mapFunc


main :: IO ()
main = defaultMain
    [ bgroup
        "day1"
        [ bench "task1"
            $ nfIO (runTaskLines "inputs/day1.txt" Day1.reader Day1.task1)
        , bench "task2"
            $ nfIO (runTaskLines "inputs/day1.txt" Day1.reader Day1.task2)
        ]
    , bgroup
        "day2"
        [ bench "task1"
            $ nfIO (runTaskLines "inputs/day2.txt" Day2.reader Day2.task1)
        , bench "task2"
            $ nfIO (runTaskLines "inputs/day2.txt" Day2.reader Day2.task2)
        ]
    , bgroup
        "day3"
        [ bench "task1"
            $ nfIO (runTaskLines "inputs/day3.txt" Day3.reader Day3.task1)
        , bench "task2"
            $ nfIO (runTaskLines "inputs/day3.txt" Day3.reader Day3.task2)
        ]
    , bgroup
        "day4"
        [ bench "task1"
            $ nfIO (runTaskLines "inputs/day4.txt" Day4.reader Day4.task1)
        , bench "task2"
            $ nfIO (runTaskLines "inputs/day4.txt" Day4.reader Day4.task2)
        ]
    , bgroup
        "day5"
        [ bench "task1"
            $ nfIO (runTask "inputs/day5.txt" Day5.reader Day5.task1)
        , bench "task2"
            $ nfIO (runTask "inputs/day5.txt" Day5.reader Day5.task2)
        , bench "task2 a bit bigger input" $ nfIO
            (runTask "inputs/day5-large1-shorter.txt" Day5.reader Day5.task2)
        ]
    , bgroup
        "day6"
        [ bench "task1"
            $ nfIO (runTask "inputs/day6.txt" Day6.reader Day6.task1)
        , bench "task2"
            $ nfIO (runTask "inputs/day6.txt" Day6.reader Day6.task2)
        ]
    , bgroup
        "day7"
        [ bench "task1"
            $ nfIO (runTask "inputs/day7.txt" Day7.reader Day7.task1)
        , bench "task2"
            $ nfIO (runTask "inputs/day7.txt" Day7.reader Day7.task2)
        ]
    , bgroup
        "day8"
        [ bench "task1"
            $ nfIO (runTask "inputs/day8.txt" Day8.reader Day8.task1)
        , bench "task2"
            $ nfIO (runTask "inputs/day8.txt" Day8.reader Day8.task2)
        ]
    , bgroup
        "day9"
        [ bench "task1"
            $ nfIO (runTaskLines "inputs/day9.txt" Day9.reader Day9.task1)
        , bench "task2"
            $ nfIO (runTaskLines "inputs/day9.txt" Day9.reader Day9.task2)
        ]
    , bgroup
        "day19"
        [ bench "task1"
            $ nfIO (runTask "inputs/day19.txt" Day19.reader Day19.task1)
        , bench "task2"
            $ nfIO (runTask "inputs/day19.txt" Day19.reader Day19.task2)
        ]
    , bgroup
        "day20"
        [ bench "task1"
            $ nfIO (runTaskLines "inputs/day20.txt" Day20.reader Day20.task1)
        , bench "task2"
            $ nfIO (runTaskLines "inputs/day20.txt" Day20.reader Day20.task2)
        ]
    ]
