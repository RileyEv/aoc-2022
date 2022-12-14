module Day10
    ( run
    , task1
    , task2
    , reader
    ) where
import           Common                         ( inputFileName
                                                , runTasks
                                                )

import           Control.Monad                  ( void )
import           Data.Array                    as A
import           Data.List                      ( foldl'
                                                , intercalate
                                                )
import           Data.List.Split                ( splitWhen )
import           Data.Sequence                 as S
                                                ( (<|)
                                                , fromList
                                                , index
                                                , mapWithIndex
                                                )
import           Language.Haskell.TH.Syntax     ( addDependentFile )
import           Text.Megaparsec
import           Text.Megaparsec.Char
import           Text.Megaparsec.Char.Lexer     ( decimal
                                                , signed
                                                )

data Command = Noop | Addx !Int deriving Show

addExecutionDelay :: [Command] -> [Command]
addExecutionDelay []            = []
addExecutionDelay (Noop   : cs) = Noop : addExecutionDelay cs
addExecutionDelay (Addx x : cs) = Noop : Addx x : addExecutionDelay cs

task1 :: [Command] -> IO (Maybe Int)
task1 commands = do
    let commandsWithExecutionDelay = addExecutionDelay commands
        xRegister = foldl' calculateXReg [1] commandsWithExecutionDelay
        xRegisterSequence = fromList xRegister
        signalStrengthSequence =
            mapWithIndex calculateSignalStrength (0 <| xRegisterSequence)

    return
        (Just
            (         signalStrengthSequence
            `S.index` 20
            +         signalStrengthSequence
            `S.index` 60
            +         signalStrengthSequence
            `S.index` 100
            +         signalStrengthSequence
            `S.index` 140
            +         signalStrengthSequence
            `S.index` 180
            +         signalStrengthSequence
            `S.index` 220
            )
        )

calculateSignalStrength :: Int -> Int -> Int
calculateSignalStrength idx x = idx * x

calculateXReg :: [Int] -> Command -> [Int]
calculateXReg [] _        = []
calculateXReg xs Noop     = xs ++ [last xs]
calculateXReg xs (Addx x) = xs ++ [last xs + x]

arrayIndicies :: (Int, Int) -> [(Int, Int)]
arrayIndicies (xMax, yMax) = [ (x, y) | y <- [0 .. yMax], x <- [0 .. xMax] ]

arrayIdxToXRegIdx :: (Int, Int) -> Int
arrayIdxToXRegIdx (x, y) = x + y * 40

task2 :: [Command] -> IO (Maybe Int)
task2 commands = do
    let commandsWithExecutionDelay = addExecutionDelay commands
        xRegister = foldl' calculateXReg [1] commandsWithExecutionDelay


    let crt = screen (39, 5) xRegister

    -- printCrt crt

    return Nothing

printCrt :: [Char] -> IO ()
printCrt crt = do
    let split      = splitWhen (\(_, i) -> i `mod` 40 == 0) (zip crt [0 ..])
        removedZip = map (map fst) split
        string     = intercalate "\n" removedZip

    putStr string

screen :: (Int, Int) -> [Int] -> [Char]
screen bound xRegister =
    [ isOn screenIdx xReg
    | (screenIdx, xReg) <- zip (arrayIndicies bound) xRegister
    ]

isOn :: (Int, Int) -> Int -> Char
isOn (x, y) spriteCenter
    | x == spriteCenter - 1 || x == spriteCenter || x == spriteCenter + 1 = '#'
    | otherwise = '.'

-- Does not need to be read, this is just some way to manipulate each line of the input file
run = runTasks (inputFileName 'run) reader task1 task2

type Parser = Parsec () String
reader :: String -> [Command]
reader str = case parse commands "inputs/day5.txt" str of
    Left  parseError -> error (show parseError)
    Right y          -> y
  where
    commands = some (command <* newline)
    command  = noop <|> addx
    noop     = Noop <$ string "noop"
    addx     = Addx <$ string "addx " <*> number

    number :: Parser Int
    number = signed (pure ()) decimal
