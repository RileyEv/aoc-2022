{-# LANGUAGE TupleSections #-}
module Day9
    ( run
    , task1
    , task2
    , reader
    ) where
import           Common                         ( inputFileName
                                                , runTasksLines
                                                )
import           Data.Foldable                  ( foldlM )
import           Data.List                      ( foldl'
                                                , nub
                                                )
import           Data.List.Extra                ( nubOrd
                                                , splitOn
                                                )
import           Prelude                 hiding ( Either(..) )

data Direction = Up | Down | Left | Right deriving Show
newtype Board = Board [(Int, Int)] deriving Show


task1 :: [[Direction]] -> IO (Maybe Int)
task1 splitDirections = do
    let directions = concat splitDirections
    (finalBoard, visitedCells) <- foldlM printPerform
                                         (Board [(0, 0), (0, 0)], [])
                                         directions
    let uniqueCells = nubOrd visitedCells
    return (Just (length uniqueCells))

printPerform :: (Board, [(Int, Int)]) -> Direction -> IO (Board, [(Int, Int)])
printPerform s dir = do
    let newS = performMove dir s
    -- print dir
    -- print newS
    -- print ""

    return newS

task2 :: [[Direction]] -> IO (Maybe Int)
task2 splitDirections = do
    let directions = concat splitDirections
    (finalBoard, visitedCells) <- foldlM printPerform
                                         (Board (replicate 10 (0, 0)), [])
                                         directions
    let uniqueCells = nubOrd visitedCells
    return (Just (length uniqueCells))



performMove :: Direction -> (Board, [(Int, Int)]) -> (Board, [(Int, Int)])
performMove dir (Board [], _) = error "cannot move a rope with no peices"
performMove dir (Board (head : rest), occupiedSquares) =
    let newHead = moveHead dir head
        newRope = (map fst . foldl' f [(newHead, head)]) rest

        -- newTail = moveTail prevHead newHead prevTail
    in  (Board newRope, last newRope : occupiedSquares)
  where
    -- Oh my this function has awful runtime complexity...
    -- repeatedly accessing the final item of a list and appending single items to the end!
    f :: [((Int, Int), (Int, Int))] -> (Int, Int) -> [((Int, Int), (Int, Int))]
    f [] _ = error "Should never be empty"
    f xs prevTail =
        let (newHead, prevHead) = last xs
        in  xs ++ [(moveTail prevHead newHead prevTail, prevTail)]


-- dont change

moveHead :: Direction -> (Int, Int) -> (Int, Int)
moveHead Left  (x, y) = (x - 1, y)
moveHead Right (x, y) = (x + 1, y)
moveHead Up    (x, y) = (x, y + 1)
moveHead Down  (x, y) = (x, y - 1)

moveTail :: (Int, Int) -> (Int, Int) -> (Int, Int) -> (Int, Int)
moveTail prevHead newHead prevTail
    | areTogether newHead prevTail = prevTail
    | otherwise                    = dragTail prevHead newHead prevTail

dragTail :: (Int, Int) -> (Int, Int) -> (Int, Int) -> (Int, Int)
dragTail (prevHeadX, prevHeadY) (newHeadX, newHeadY) (prevTailX, prevTailY)
    | newHeadX == prevTailX && newHeadY > prevTailY = (prevTailX, prevTailY + 1)
    | newHeadX == prevTailX && newHeadY < prevTailY = (prevTailX, prevTailY - 1)
    | newHeadY == prevTailY && newHeadX > prevTailX = (prevTailX + 1, prevTailY)
    | newHeadY == prevTailY && newHeadX < prevTailX = (prevTailX - 1, prevTailY)
    | otherwise = moveDiagonally (newHeadX, newHeadY) (prevTailX, prevTailY)
    -- | otherwise = (prevHeadX, prevHeadY)

moveDiagonally :: (Int, Int) -> (Int, Int) -> (Int, Int)
moveDiagonally newHead (prevTailX, prevTailY) =
    let [move] =
            [ move
            | move <-
                [ (prevTailX - 1, prevTailY - 1)
                , (prevTailX + 1, prevTailY - 1)
                , (prevTailX + 1, prevTailY + 1)
                , (prevTailX - 1, prevTailY + 1)
                ]
            , areTogether newHead move
            ]
    in  move

areTogether :: (Int, Int) -> (Int, Int) -> Bool
areTogether (newHeadX, newHeadY) (prevTailX, prevTailY) =
    abs (newHeadX - prevTailX) <= 1 && abs (newHeadY - prevTailY) <= 1

-- Does not need to be read, this is just some way to manipulate each line of the input file
run = runTasksLines (inputFileName 'run) reader task1 task2


reader :: String -> [Direction]
reader str = case splitOn " " str of
    []       -> error "eh??"
    [x]      -> error "ehhhh?????"
    [x, y]   -> replicate (read y) (strToDirection x)
    _3OfMore -> error "ehhh???"

strToDirection :: String -> Direction
strToDirection "U"        = Up
strToDirection "D"        = Down
strToDirection "L"        = Left
strToDirection "R"        = Right
strToDirection _otherChar = error "Unknown direction"
