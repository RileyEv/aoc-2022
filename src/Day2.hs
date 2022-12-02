module Day2
    ( run
    , task1
    , task2
    , Input1(..)
    , Input2(..)
    ) where
import           Common                         ( inputFileName
                                                , runTasksLines
                                                , split
                                                )
import           Data.Tuple.Extra               ( (***) )

data Input1 = A | B | C deriving Show
data Input2 = X | Y | Z deriving Show

data Throw = Rock | Paper | Scissors deriving Show
data Result = Lose | Draw | Win deriving Show

reader :: String -> (Input1, Input2)
reader str = (firstPartParsed, secondPartParsed)
  where
    [firstPart, secondPart] = split ' ' str
    firstPartParsed         = case firstPart of
        "A" -> A
        "B" -> B
        "C" -> C
        _   -> error "What happened here?!"
    secondPartParsed = case secondPart of
        "X" -> X
        "Y" -> Y
        "Z" -> Z
        _   -> error "What happened here?!"

input1Mapping :: Input1 -> Throw
input1Mapping A = Rock
input1Mapping B = Paper
input1Mapping C = Scissors

input2ThrowMapping :: Input2 -> Throw
input2ThrowMapping X = Rock
input2ThrowMapping Y = Paper
input2ThrowMapping Z = Scissors

input2ResultMapping :: Input2 -> Result
input2ResultMapping X = Lose
input2ResultMapping Y = Draw
input2ResultMapping Z = Win

score :: Throw -> Throw -> Int
-- | their throw -> my throw -> score = shapeScore + outcomeScore
score Rock     Rock     = 1 + 3
score Paper    Rock     = 1 + 0
score Scissors Rock     = 1 + 6
score Rock     Paper    = 2 + 6
score Paper    Paper    = 2 + 3
score Scissors Paper    = 2 + 0
score Rock     Scissors = 3 + 0
score Paper    Scissors = 3 + 6
score Scissors Scissors = 3 + 3

task1 :: [(Input1, Input2)] -> IO (Maybe Int)
task1 input = do
    let games = map (input1Mapping *** input2ThrowMapping) input
    return (Just (sum (map (uncurry score) games)))


newScore :: Throw -> Result -> Int
-- their throw -> result -> score = shapeScore + outcomeScore
newScore Rock     Lose = 3 + 0
newScore Rock     Draw = 1 + 3
newScore Rock     Win  = 2 + 6
newScore Paper    Lose = 1 + 0
newScore Paper    Draw = 2 + 3
newScore Paper    Win  = 3 + 6
newScore Scissors Lose = 2 + 0
newScore Scissors Draw = 3 + 3
newScore Scissors Win  = 1 + 6


task2 :: [(Input1, Input2)] -> IO (Maybe Int)
task2 input = do
    let games = map (input1Mapping *** input2ResultMapping) input

    return (Just (sum (map (uncurry newScore) games)))

-- Does not need to be read, this is just some way to manipulate each line of the input file
run = runTasksLines (inputFileName 'run) reader task1 task2
