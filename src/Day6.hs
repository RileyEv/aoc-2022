{-# LANGUAGE LambdaCase #-}
module Day6
    ( run
    , task1
    , task2
    , reader
    ) where
import           Common                         ( inputFileName
                                                , runTasks
                                                )
import           Data.List.Unique               ( allUnique )
import qualified Data.Map                      as Map
                                                ( Map
                                                , alter
                                                , empty
                                                , fromList
                                                , insert
                                                , size
                                                , update
                                                )
import qualified Data.Vector                   as Vec
                                                ( (!)
                                                , Vector
                                                , empty
                                                , fromList
                                                , head
                                                , tail
                                                )

-- | findStart prefixLength cs
--   length cs ~ m
--   prefixLength ~ n
--   O(mnlog(n))
findStart :: Int -> [Char] -> Maybe Int
findStart _ [] = Nothing --error "Ran out of characters"
findStart n cs@(_ : cs') =
    let start  = take n cs
        unique = allUnique start
    in  if unique then Just n else (+ 1) <$> findStart n cs'

-- | findStart prefixLength cs
--   length cs ~ m
--   prefixLength ~ n
--   O((m+n)log(n))
findStart' :: Int -> Vec.Vector Char -> Map.Map Char Int -> Maybe Int
findStart' n cs map =
    let c        = Vec.head cs
        cs'      = Vec.tail cs
        nextChar = cs Vec.! (n - 1)
        map'     = insertChar nextChar map
        unique   = isPrefix n map'
    in  if unique
            then Just n
            else (+ 1) <$> findStart' n cs' (removeChar c map')




-- | O(1)
isPrefix :: Int -> Map.Map Char Int -> Bool
isPrefix n = (== n) . Map.size

-- | O(log(n))
insertChar :: Char -> Map.Map Char Int -> Map.Map Char Int
insertChar = Map.alter
    (\case
        Nothing -> Just 1
        Just n  -> Just (n + 1)
    )

-- | O(log(n))
removeChar :: Char -> Map.Map Char Int -> Map.Map Char Int
removeChar = Map.update (\n -> if n <= 1 then Nothing else Just (n - 1))


task1 :: String -> IO (Maybe Int)
task1 str = do
    let vec = Vec.fromList str
    let map = foldr insertChar Map.empty (take 3 str)
    let n   = findStart' 4 vec map
    return n

task2 :: String -> IO (Maybe Int)
task2 str = do
    let vec = Vec.fromList str
    let map = foldr insertChar Map.empty (take 13 str)
    let n   = findStart' 14 vec map
    return n

-- Does not need to be read, this is just some way to manipulate each line of the input file
run = runTasks (inputFileName 'run) reader task1 task2

reader :: String -> String
reader = id
