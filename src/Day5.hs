{-# LANGUAGE LambdaCase #-}
module Day5
    ( run
    , task1
    , task2
    , Stack(..)
    , Move(..)
    , Mission(..)
    ) where
import           Common                         ( inputFileName
                                                , runTasks
                                                )

import           Control.Applicative            ( Alternative(some) )
import           Data.List                      ( transpose )
import           Data.Map                       ( Map
                                                , empty
                                                , fromList
                                                , insert
                                                , toAscList
                                                , update
                                                )
import           Data.Maybe                     ( catMaybes )
import           Data.Vector
import           Text.Parsec                    ( (<|>)
                                                , between
                                                , char
                                                , digit
                                                , many
                                                , newline
                                                , oneOf
                                                , optional
                                                , parse
                                                , string
                                                , try
                                                )

newtype Stack = Stack Int deriving (Eq, Ord, Show)

data Move = Move Int Int Int
    deriving Show

data Mission = Mission (Vector [Char]) [Move]
    deriving Show


applyMove :: Bool -> Vector [Char] -> Move -> Vector [Char]
applyMove flip stacks move@(Move n from to) =
    let
        fromStack               = stacks ! (from - 1)
        toStack                 = stacks ! (to - 1)
        (takenItems, leftItems) = Prelude.splitAt n fromStack
        newToStack =
            (if flip then Prelude.reverse else id) takenItems Prelude.++ toStack
    in
        stacks // [(from - 1, leftItems), (to - 1, newToStack)]


task1 :: Mission -> IO (Maybe String)
task1 (Mission stacks moves) = do
    let completeState = Prelude.foldl (applyMove True) stacks moves
    return (Just (toList (Data.Vector.map Prelude.head completeState)))

spaceHead :: String -> Char
spaceHead []      = ' '
spaceHead (x : _) = x

task2 :: Mission -> IO (Maybe String)
task2 (Mission stacks moves) = do
    let completeState = Prelude.foldl (applyMove False) stacks moves
    return (Just (toList (Data.Vector.map Prelude.head completeState)))

-- Does not need to be read, this is just some way to manipulate each line of the input file
run = runTasks (inputFileName 'run) reader task1 task2


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
    number = read <$> many digit
    buildStacks :: [[Maybe Char]] -> [Int] -> Vector [Char]
    buildStacks items _ = Data.Vector.fromList
        (Prelude.map Data.Maybe.catMaybes (transpose items))
