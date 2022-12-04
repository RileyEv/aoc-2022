module Day3
    ( run
    , task1
    , task2
    ) where
import           Common                         ( inputFileName
                                                , runTasksLines
                                                )

import           Data.Char                      ( ord )
import           Data.List.Extra                ( chunksOf )
import           Data.Set                       ( elems
                                                , empty
                                                , fromList
                                                , intersection
                                                )
-- countBagAndObjects :: String -> (Int, Map Char Int)

data Bag = Bag Int String String

splitToBag :: String -> Bag
splitToBag str =
    let len = length str `div` 2 in Bag len (take len str) (drop len str)

findCommonItem :: Bag -> Char
findCommonItem (Bag _ c1 c2) =
    let s1 = fromList c1
        s2 = fromList c2
        i  = intersection s1 s2
    in  head (elems i)

findCommonElfItem :: [Bag] -> Char
findCommonElfItem bags = head (elems (go bags))
  where
    go = foldr f k
    f (Bag _ c1 c2) s = intersection (fromList (c1 ++ c2)) s
    k = fromList (['a' .. 'z'] ++ ['A' .. 'Z'])


getPriority :: Char -> Int
getPriority c | 'a' <= c && c <= 'z' = ord c - 96
              | 'A' <= c && c <= 'Z' = ord c - 38
              | otherwise            = error "Eh?!"

task1 :: [String] -> IO (Maybe Int)
task1 dirtyBags = do
    let bags        = map splitToBag dirtyBags
    let commonItems = map findCommonItem bags
    let priorities  = map getPriority commonItems

    return $ Just (sum priorities)

task2 :: [String] -> IO (Maybe Int)
task2 dirtyBags = do
    let bags        = map splitToBag dirtyBags
    let elfGroups   = chunksOf 3 bags
    let commonItems = map findCommonElfItem elfGroups
    let priorities  = map getPriority commonItems

    return $ Just (sum priorities)

-- Does not need to be read, this is just some way to manipulate each line of the input file
run = runTasksLines (inputFileName 'run) id task1 task2
