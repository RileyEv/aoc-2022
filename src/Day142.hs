{-# LANGUAGE Strict #-}
module Day142
    ( run
    , task1
    , task2
    , reader
    ) where
import           Common                         ( inputFileName
                                                , runTasks
                                                )
import           Data.Array                     ( (!)
                                                , (//)
                                                , Array
                                                , array
                                                )
import           Data.Bifunctor                 ( bimap )
import qualified Data.Map                      as M
import qualified Data.Set                      as S
import           Text.Megaparsec                ( Parsec
                                                , parse
                                                , sepBy1
                                                , sepEndBy1
                                                , some
                                                )
import           Text.Megaparsec.Char           ( char
                                                , digitChar
                                                , newline
                                                , string
                                                )

data CellType = Sand | Rock | Air deriving (Show, Eq)
type Grid = M.Map (Int, Int) CellType


locDown :: (Int, Int) -> (Int, Int)
locDown (x, y) = (x, y + 1)

locDiagLeft :: (Int, Int) -> (Int, Int)
locDiagLeft (x, y) = (x - 1, y + 1)

locDiagRight :: (Int, Int) -> (Int, Int)
locDiagRight (x, y) = (x + 1, y + 1)

isFree :: Grid -> (Int, Int) -> Bool
isFree grid loc = not (loc `M.member` grid) --if M.member p grid then Nothing else Just p


expandTraces :: [[(Int, Int)]] -> [(Int, Int)]
expandTraces = concatMap expandTrace

expandTrace :: [(Int, Int)] -> [(Int, Int)]
expandTrace []  = []
expandTrace [x] = [x]
expandTrace ((x1, x2) : y@(y1, y2) : xs) =
    [ (a, b)
    | a <- [(min x1 y1) .. (max x1 y1)]
    , b <- [(min x2 y2) .. (max x2 y2)]
    ]
    ++ expandTrace (y : xs)

-- * Task 1

task1 :: [[(Int, Int)]] -> IO (Maybe Int)
task1 traces = do
    (maxY, grid) <- tracesToGrid traces

    let (finalGrid, n) = doTask1 maxY grid
    let (firstGrid, _) = dropGrain maxY (500, 0) grid

    return (Just n)

doTask1 :: Int -> Grid -> (Grid, Int)
doTask1 maxY grid =
    let oneGrain = dropGrain maxY (500, 0) grid
    in  case oneGrain of
            (grid, Nothing) -> (grid, 0)
            (grid, _      ) -> fmap (+ 1) (doTask1 maxY grid)


dropGrain :: Int -> (Int, Int) -> Grid -> (Grid, Maybe CellType)
dropGrain maxY loc@(x, y) grid = if y == maxY
    then (grid, Nothing)
    else
        let down       = locDown loc
            diagLeft   = locDiagLeft loc
            diagRight  = locDiagRight loc
            moveToCell = if isFree grid down
                then Just down
                else
                    (if isFree grid diagLeft
                        then Just diagLeft
                        else
                            (if isFree grid diagRight
                                then Just diagRight
                                else Nothing
                            )
                    )
        in  case moveToCell of
                Just cell -> dropGrain maxY cell grid
                Nothing   -> (M.insert loc Sand grid, Just Air)

tracesToGrid :: [[(Int, Int)]] -> IO (Int, Grid)
tracesToGrid traces = do
    let rocks   = expandTraces traces
        yValues = 0 : map snd rocks
    return (maximum yValues + 1, M.fromList (zip rocks (repeat Rock)))


-- * Task 2

task2 :: [[(Int, Int)]] -> IO (Maybe Int)
task2 traces = do
    (maxY, grid) <- tracesToGrid traces

    let (finalGrid, n) = doTask2 maxY grid
    print finalGrid

    return (Just n)

doTask2 :: Int -> Grid -> (Grid, Int)
doTask2 maxY grid =
    let oneGrain = dropGrain2 maxY (500, 0) grid
    in  case oneGrain of
            (grid, Nothing) -> (grid, 0)
            (grid, _      ) -> fmap (+ 1) (doTask2 maxY grid)


dropGrain2 :: Int -> (Int, Int) -> Grid -> (Grid, Maybe CellType)
dropGrain2 maxY loc@(x, y) grid = if y == maxY + 1
    then (M.insert loc Sand grid, Just Air)
    else
        let down       = locDown loc
            diagLeft   = locDiagLeft loc
            diagRight  = locDiagRight loc
            moveToCell = if isFree grid down
                then Just down
                else
                    (if isFree grid diagLeft
                        then Just diagLeft
                        else
                            (if isFree grid diagRight
                                then Just diagRight
                                else Nothing
                            )
                    )
        in  case moveToCell of
                Just cell -> dropGrain maxY cell grid
                Nothing   -> if loc == (500, 0)
                    then (grid, Nothing)
                    else (M.insert loc Sand grid, Just Air)

-- dropGrain2 :: (Int, Int) -> Grid -> (Grid, CellType)
-- dropGrain2 loc grid =
--     let down       = locDown loc
--         diagLeft   = locDiagLeft loc
--         diagRight  = locDiagRight loc
--         moveToCell = {-# SCC moveToCell #-} if isFree grid down
--             then Just down
--             else
--                 (if isFree grid diagLeft
--                     then Just diagLeft
--                     else
--                         (if isFree grid diagRight
--                             then Just diagRight
--                             else Nothing
--                         )
--                 )
--     in  case moveToCell of
--             Just cell -> dropGrain2 cell grid
--             Nothing   -> if loc == (500, 0)
--                 then (grid, Ether)
--                 else {-# SCC insertToGrid #-}(grid // [(loc, Sand)], grid ! loc)

-- doTask2 :: Grid -> (Grid, Int)
-- doTask2 grid =
--     let oneGrain = dropGrain2 (500, 0) grid
--     in  case oneGrain of
--             (grid, Ether) -> (grid, 1)
--             (grid, _    ) -> fmap (+ 1) (doTask2 grid)


-- tracesToGrid2 :: [[(Int, Int)]] -> IO Grid
-- tracesToGrid2 traces = do
--     let expandedTraces = expandTraces traces
--         rocks          = concat expandedTraces
--     let xValues = 500 : map fst rocks
--         yValues = 0 : map snd rocks
--         bounds =
--             ( (minimum xValues, minimum yValues)
--             , (maximum xValues, maximum yValues)
--             )
--     let slightlyExpandedBounds =
--             bimap (bimap (+ (-200)) (+ (-1))) (bimap (+ 200) (+ 2)) bounds
--         rocksSet = S.fromList rocks
--     return
--         (array
--             slightlyExpandedBounds
--             [ (idx, cellType2 slightlyExpandedBounds rocksSet idx)
--             | idx <- arrayIndices slightlyExpandedBounds
--             ]
--         )


-- cellType2
--     :: ((Int, Int), (Int, Int)) -> S.Set (Int, Int) -> (Int, Int) -> CellType
-- cellType2 (_, (_, yMax)) rocksSet idx@(_, y) | idx `S.member` rocksSet = Rock
--                                              | y == yMax               = Rock
--                                              | otherwise               = Air


-- Does not need to be read, this is just some way to manipulate each line of the input file
run = runTasks (inputFileName 'run) reader task1 task2

type Parser = Parsec () String
reader :: String -> [[(Int, Int)]]
reader str = case parse traces "inputs/day13.txt" str of
    Left  parseError -> error (show parseError)
    Right y          -> y
  where
    traces :: Parser [[(Int, Int)]]
    traces = sepEndBy1 trace newline

    trace :: Parser [(Int, Int)]
    trace = sepBy1 pair (string " -> ")

    pair :: Parser (Int, Int)
    pair = (,) <$> number <* char ',' <*> number

    number :: Parser Int
    number = read <$> some digitChar
