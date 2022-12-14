module Day12
    ( run
    , task1
    , task2
    , reader
    ) where
import           Common                         ( inputFileName
                                                , runTasks
                                                )
import           Data.Array
import           Data.Char                      ( chr
                                                , ord
                                                )
import           Data.Foldable                  ( foldrM )
import           Data.List                      ( find )
import           Data.Set

findS :: Array (Int, Int) Char -> (Int, Int)
findS arr = case find (\(_idx, value) -> value == 'S') (assocs arr) of
    Just (idx, _value) -> idx
    Nothing            -> error "Could not find S!"

getPossibleLookups
    :: (Int, Int)
    -> ((Int, Int), (Int, Int))
    -> Set (Int, Int)
    -> Array (Int, Int) Char
    -> [(Int, Int)]
getPossibleLookups pos@(x, y) ((xMin, yMin), (xMax, yMax)) visited arr =
    Prelude.filter
        (\pos'@(x', y') ->
            xMin
                <=          x'
                &&          x'
                <=          xMax
                &&          yMin
                <=          y'
                &&          y'
                <=          yMax
                &&          pos'
                `notMember` visited
                && (let val  = if arr ! pos == 'S' then 'a' else arr ! pos
                        val' = if arr ! pos' == 'E' then 'z' else arr ! pos'
                    in  val == val' || val == chr (ord val' - 1)
                   )
        )
        [(x + 1, y), (x, y + 1), (x - 1, y), (x, y - 1)]

findPaths
    :: Array (Int, Int) Char
    -> Set (Int, Int)
    -> (Int, Int)
    -> IO ([[(Int, Int)]], Set (Int, Int))
findPaths arr visited pos = if arr ! pos == 'E'
    then return ([[pos]], pos `insert` visited)
    else do
        let newVisited = pos `insert` visited
            lookups    = getPossibleLookups pos (bounds arr) newVisited arr

        -- print (pos, lookups)
        (subPaths, visited') <- foldrM f ([], newVisited) lookups

        let subPaths' = Prelude.map (pos :) subPaths

        return (subPaths', newVisited `Data.Set.union` visited')
  where
    f
        :: (Int, Int)
        -> ([[(Int, Int)]], Set (Int, Int))
        -> IO ([[(Int, Int)]], Set (Int, Int))
    f pos (subPaths, visited) = do
        (path, visited') <- findPaths arr visited pos

        return (path ++ subPaths, visited')


task1 :: Array (Int, Int) Char -> IO (Maybe Int)
task1 arr = do
    let start = findS arr
    -- print arr
    (path, visited) <- findPaths arr empty start
    -- print arr
    -- print path
    return Nothing -- (Just (length (head path) - 1))

task2 :: Array (Int, Int) Char -> IO (Maybe Int)
task2 _ = return Nothing

-- Does not need to be read, this is just some way to manipulate each line of the input file
run = runTasks (inputFileName 'run) reader task1 task2

reader :: String -> Array (Int, Int) Char
reader str =
    let rows = lines str
        values =
            [ (x, y)
            | (rowNumber, row) <- zip [0 ..] rows
            , (x, y) <- [ ((x, rowNumber), char) | (x, char) <- zip [0 ..] row ]
            ]
        firstValue = (fst . head) values
        lastValue  = (fst . last) values
    in  array (firstValue, lastValue) values
