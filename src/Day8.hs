module Day8
    ( run
    , task1
    , task2
    , reader
    , howFarCanISee
    , countViewingDistance
    , getRows
    , getColumns
    ) where
import           Common                         ( inputFileName
                                                , runTasks
                                                )

import           Data.Array
import           Data.List.Extra                ( maximumOn )
import qualified Data.Set                      as S
                                                ( fromList
                                                , size
                                                )

arrayIndicies :: (Int, Int) -> [(Int, Int)]
arrayIndicies gridSize =
    [ (x, y) | x <- [0 .. (fst gridSize)], y <- [0 .. (snd gridSize)] ]

traverseUpDownLeftRight :: Array (Int, Int) Int -> Int
traverseUpDownLeftRight aHeights = (S.size . S.fromList)
    (  concatMap (fst . foldr countVisibleTrees ([], -1)) (getColumns aHeights)
    ++ concatMap (fst . foldr countVisibleTrees ([], -1) . reverse)
                 (getColumns aHeights)

    ++ concatMap (fst . foldr countVisibleTrees ([], -1)) (getRows aHeights)
    ++ concatMap (fst . foldr countVisibleTrees ([], -1) . reverse)
                 (getRows aHeights)
    )


traverseViewDown :: Array (Int, Int) Int -> Array (Int, Int) Int
traverseViewDown arr = array
    (bounds arr)
    [ (a, c)
    | (a, _, c) <- concatMap (foldr countViewingDistance [] . reverse)
                             (getColumns arr)
    ]

traverseViewUp :: Array (Int, Int) Int -> Array (Int, Int) Int
traverseViewUp arr = array
    (bounds arr)
    [ (a, c)
    | (a, _, c) <- concatMap (foldr countViewingDistance []) (getColumns arr)
    ]

traverseViewLeft :: Array (Int, Int) Int -> Array (Int, Int) Int
traverseViewLeft arr = array
    (bounds arr)
    [ (a, c)
    | (a, _, c) <- concatMap (foldr countViewingDistance []) (getRows arr)
    ]


traverseViewRight :: Array (Int, Int) Int -> Array (Int, Int) Int
traverseViewRight arr = array
    (bounds arr)
    [ (a, c)
    | (a, _, c) <- concatMap (foldr countViewingDistance [] . reverse)
                             (getRows arr)
    ]


countViewingDistance
    :: ((Int, Int), Int) -> [((Int, Int), Int, Int)] -> [((Int, Int), Int, Int)]
countViewingDistance (idx, height) [] = [(idx, height, 0)]
countViewingDistance (idx, height) xs =
    (idx, height, howFarCanISee height xs) : xs

howFarCanISee :: Int -> [((Int, Int), Int, Int)] -> Int
howFarCanISee myHeight [] = 0
howFarCanISee myHeight ((_, height, _) : xs) =
    if myHeight > height then 1 + howFarCanISee myHeight xs else 1

countVisibleTrees
    :: ((Int, Int), Int) -> ([(Int, Int)], Int) -> ([(Int, Int)], Int) -- (count, heightest height)
countVisibleTrees (idx, height) (idxs, highestHeight) =
    if height > highestHeight
        then (idx : idxs, height)
        else (idxs, highestHeight)

getRows :: Array (Int, Int) Int -> [[((Int, Int), Int)]]
getRows array =
    let (min, max) = bounds array
    in  [ [ ((x, y), array ! (x, y)) | y <- [0 .. snd max] ]
        | x <- [0 .. fst max]
        ]

getColumns :: Array (Int, Int) Int -> [[((Int, Int), Int)]]
getColumns array =
    let (min, max) = bounds array
    in  [ [ ((x, y), array ! (x, y)) | x <- [0 .. fst max] ]
        | y <- [0 .. snd max]
        ]


-- printGrid :: Array (Int, Int) Int -> IO ()
-- printGrid arr = do
--     let (_, size) = bounds arr
--     let string = foldl' (thing size) "" (arrayIndiciesT size)
--     putStr string
--   where
--     thing :: (Int, Int) -> String -> (Int, Int) -> String
--     thing size str idx@(x, y) =
--         str ++ show (arr ! (x, y)) ++ if fst idx == fst size then "\n" else ""

prodGrids
    :: Array (Int, Int) Int
    -> Array (Int, Int) Int
    -> Array (Int, Int) Int
    -> Array (Int, Int) Int
    -> Array (Int, Int) Int
prodGrids up down left right = array
    (bounds up)
    [ (idx, up ! idx * down ! idx * left ! idx * right ! idx)
    | idx <- arrayIndicies ((snd . bounds) up)
    ]


task1 :: Array (Int, Int) Int -> IO (Maybe Int)
task1 grid = do
    let visible = traverseUpDownLeftRight grid
    let rows = getRows grid
        row  = rows !! 0
    return (Just visible)

task2 :: Array (Int, Int) Int -> IO (Maybe Int)
task2 grid = do
    let right'      = traverseViewUp grid
        left'       = traverseViewDown grid
        down'       = traverseViewLeft grid
        up'         = traverseViewRight grid
        scenicScore = prodGrids right' left' down' up'

    let (maxIdx, val) = (maximumOn snd . assocs) scenicScore

    return (Just val)


-- Does not need to be read, this is just some way to manipulate each line of the input file
run = runTasks (inputFileName 'run) reader task1 task2

reader :: String -> Array (Int, Int) Int
reader str =
    let
        rows = lines str
        values =
            [ (x, y)
            | (rowNumber, row) <- zip [0 ..] rows
            , (x        , y  ) <-
                [ ((x, rowNumber), read [height])
                | (x, height) <- zip [0 ..] row
                ]
            ]
        firstValue = (fst . head) values
        lastValue  = (fst . last) values
    in
        array (firstValue, lastValue) values
