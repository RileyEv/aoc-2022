{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE Strict #-}

module Day5
    ( run
    , task1
    , task2
    , reader
    , Move(..)
    , Mission(..)
    ) where
import           Common                         ( inputFileName
                                                , runTasks
                                                )

import           Control.Applicative            ( Alternative(some) )
import           Data.List                      ( foldl'
                                                , transpose
                                                )
import           Data.Maybe                     ( catMaybes )
import           Data.Vector                    ( (!)
                                                , (++)
                                                , (//)
                                                , Vector
                                                , fromList
                                                , head
                                                , map
                                                , reverse
                                                , splitAt
                                                , toList
                                                )
import           Data.Vector.Unboxed            ( (!)
                                                , (++)
                                                , (//)
                                                , Vector
                                                , fromList
                                                , head
                                                , map
                                                , reverse
                                                , splitAt
                                                , toList
                                                )
import           Data.Void                      ( Void )
import           Text.Megaparsec                ( (<|>)
                                                , Parsec
                                                , between
                                                , many
                                                , oneOf
                                                , optional
                                                , parse
                                                , sepBy1
                                                , try
                                                )
import           Text.Megaparsec.Char           ( char
                                                , digitChar
                                                , newline
                                                , string
                                                )


data Move = Move !Int !Int !Int
    deriving Show

data Mission = Mission !(Data.Vector.Vector (Data.Vector.Unboxed.Vector Char))
                       ![Move]
    deriving Show


applyMove
    :: (Data.Vector.Unboxed.Vector Char -> Data.Vector.Unboxed.Vector Char)
    -> Data.Vector.Vector (Data.Vector.Unboxed.Vector Char)
    -> Move
    -> Data.Vector.Vector (Data.Vector.Unboxed.Vector Char)
applyMove f stacks move@(Move n from to) =
    let
        fromStack = {-# SCC "applyMove.fromStack" #-} stacks Data.Vector.! (from - 1)
        toStack   = {-# SCC "applyMove.toStack" #-}stacks Data.Vector.! (to - 1)
        (takenItems, leftItems) =
            {-# SCC "applyMove.splitAt" #-} Data.Vector.Unboxed.splitAt n fromStack
        newToStack =
            {-# SCC "applyMove.append" #-}f takenItems Data.Vector.Unboxed.++ toStack
    in
        {-# SCC "applyMove.insert" #-} stacks Data.Vector.// [(from - 1, leftItems), (to - 1, newToStack)]


task1 :: Mission -> IO (Maybe String)
task1 (Mission stacks moves) = do
    let completeState =
            foldl' (applyMove Data.Vector.Unboxed.reverse) stacks moves
    return
        (Just
            (Data.Vector.toList
                (Data.Vector.map Data.Vector.Unboxed.head completeState)
            )
        )

task2 :: Mission -> IO (Maybe String)
task2 (Mission stacks moves) = do
    let completeState = foldl' (applyMove id) stacks moves
    return
        (Just
            (Data.Vector.toList
                (Data.Vector.map Data.Vector.Unboxed.head completeState)
            )
        )

-- Does not need to be read, this is just some way to manipulate each line of the input file
run = runTasks (inputFileName 'run) reader task1 task2

type Parser = Parsec Void String

reader :: String -> Mission
reader str = case parse mission "inputs/day5.txt" str of
    Left  parseError -> error (show parseError)
    Right y          -> y
  where
    mission      = Mission <$> stacks <* newline <*> moves
    stacks       = buildStacks <$> items <*> names
    names        = some (name <* optional (char ' ')) <* newline
    name         = between (char ' ') (char ' ') number
    items        = some maybeItemRow
    maybeItemRow = some maybeItem <* newline
    maybeItem    = Just <$> item <|> Nothing <$ notItem
    notItem      = try (string "   ") *> optional (char ' ')
    item = char '[' *> oneOf ['A' .. 'Z'] <* char ']' <* optional (char ' ')
    moves        = many (move <* optional newline)
    move =
        Move
            <$  string "move "
            <*> number
            <*  string " from "
            <*> number
            <*  string " to "
            <*> number
    number :: Parser Int
    number = read <$> many digitChar
    buildStacks
        :: [[Maybe Char]]
        -> [Int]
        -> Data.Vector.Vector (Data.Vector.Unboxed.Vector Char)
    buildStacks items _ = Data.Vector.fromList
        (Prelude.map (Data.Vector.Unboxed.fromList . Data.Maybe.catMaybes)
                     (transpose items)
        )
