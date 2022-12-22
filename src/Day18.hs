module Day18
    ( run
    , task1
    , task2
    , reader
    ) where
import           Common                         ( inputFileName
                                                , runTasksLines
                                                , split
                                                )
import           Data.List.Unique               ( count )

type Pos = (Int, Int, Int)

task1 :: [Pos] -> IO (Maybe Int)
task1 pos = do
    -- print pos
    let xDir = (count . map (\(a, b, c) -> (b, c))) pos
        yDir = (count . map (\(a, b, c) -> (a, c))) pos
        zDir = (count . map (\(a, b, c) -> (a, b))) pos
    -- print xDir
    -- print yDir
    -- print zDir

    let surfaceArea = (length xDir + length yDir + length zDir) * 2

    return (Just surfaceArea)

task2 :: [Pos] -> IO (Maybe Int)
task2 _ = return Nothing

-- Does not need to be read, this is just some way to manipulate each line of the input file
run = runTasksLines (inputFileName 'run) reader task1 task2

reader :: String -> Pos
reader str = let (x : y : z : []) = split ',' str in (read x, read y, read z)
