module Day1
    ( run
    , task1
    , task2
    , reader
    ) where
import           Common                         ( inputFileName
                                                , runTasksLines
                                                )
import           Data.List                      ( sortBy )
import           Data.Ord                       ( Down(..)
                                                , comparing
                                                )
import           Text.Read                      ( readMaybe )

-- | Group inputs into each elf
organiseInputs :: [Maybe Int] -> [[Int]]
organiseInputs = foldr f k
  where
    f :: Maybe Int -> [[Int]] -> [[Int]]
    f (Just x) []         = [[x]]
    f (Just x) (xs : xss) = (x : xs) : xss
    f Nothing  xss        = [] : xss
    k = []




task1 :: [Maybe Int] -> IO (Maybe Int)
task1 messyInputs = do
    let inputs = organiseInputs messyInputs
    let values = map sum inputs
    return (Just (maximum values))

task2 :: [Maybe Int] -> IO (Maybe Int)
task2 messyInputs = do
    let inputs = organiseInputs messyInputs
    let values = sortBy (comparing Down) (map sum inputs)
    return (Just (sum (take 3 values)))

-- Does not need to be read, this is just some way to manipulate each line of the input file
run = runTasksLines (inputFileName 'run) reader task1 task2

reader :: String -> Maybe Int
reader = readMaybe
