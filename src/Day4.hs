{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}
module Day4
    ( run
    , task1
    , task2
    , task3
    , ElfPair(..)
    , ElfWork(..)
    , pairIntersections
    , workSorting
    , removeUnrelevant
    ) where
import           Common                         ( inputFileName
                                                , runTasksLines
                                                )
import           Data.Either                    ( partitionEithers )
import           Data.List                      ( sortBy )
import           Text.Parsec

data ElfWork = ElfWork Int Int
    deriving Show
data ElfPair = ElfPair ElfWork ElfWork
    deriving Show

sumBool :: [Bool] -> Int
sumBool = foldr
    (\case
        True  -> (+ 1)
        False -> id
    )
    0


-- * Task 1

containedWithin :: ElfPair -> Bool
containedWithin (ElfPair l r) = l `contains` r || r `contains` l

contains :: ElfWork -> ElfWork -> Bool
-- Is the first elf inside the second elf?
contains (ElfWork a b) (ElfWork x y) = x <= a && b <= y

task1 :: [ElfPair] -> IO (Maybe Int)
task1 pairs = return (Just (sumBool (map containedWithin pairs)))


-- * Task 2

doesPairIntersect :: ElfPair -> Bool
doesPairIntersect p@(ElfPair (ElfWork a b) (ElfWork x y)) =
    containedWithin p || x <= a && a <= y || x <= b && b <= y

task2 :: [ElfPair] -> IO (Maybe Int)
task2 pairs = return (Just (sumBool (map doesPairIntersect pairs)))


-- * Task 3

-- | Becasue I didn't read the question very well I actually
--   did different task for task2....
--
--   This task checks to see how many pairs of elves intesect with
--   another pair of elves
task3 :: [ElfPair] -> IO (Maybe Int)
task3 pairs = do
    let mergedPairs       = map mergePair pairs
    let sortedPairs       = sortBy workSorting mergedPairs
    let intersectingPairs = pairIntersections sortedPairs
    return (Just (length intersectingPairs))

-- | Get the number of intersections of an elvish workload
pairIntersections :: [ElfWork] -> [(ElfWork, ElfWork)]
pairIntersections = go []
  -- where
go :: [ElfWork] -> [ElfWork] -> [(ElfWork, ElfWork)]
go _relevantPairs [] = []
go relevantPairs (p : ps) =
    howManyPairsIntersect relevantPairs p
        ++ go (p : removeUnrelevant p relevantPairs) ps

removeUnrelevant :: ElfWork -> [ElfWork] -> [ElfWork]
removeUnrelevant (ElfWork a _) = filter (\(ElfWork x y) -> a < y)

howManyPairsIntersect :: [ElfWork] -> ElfWork -> [(ElfWork, ElfWork)]
howManyPairsIntersect pairs elfPair =
    map (, elfPair) (filter (`doesIntersect` elfPair) pairs)

doesIntersect :: ElfWork -> ElfWork -> Bool
doesIntersect p1@(ElfWork a b) p2@(ElfWork x _) =
    a < x && b > x || contains p1 p2

workSorting :: ElfWork -> ElfWork -> Ordering
workSorting (ElfWork a b) (ElfWork x y) | a < x           = LT
                                        | a > x           = GT
                                        | a == x && b < y = LT
                                        | a == x && b > y = GT
                                        | otherwise       = EQ

mergePair :: ElfPair -> ElfWork
mergePair (ElfPair (ElfWork a b) (ElfWork x y)) = ElfWork (max a x) (max b y)

run = runTasksLines (inputFileName 'run) reader task1 task2

reader :: String -> ElfPair
reader str = case parse parser "" str of
    Left  _ -> error "couldn't parse !?"
    Right y -> y
  where
    parser  = ElfPair <$> elfWork <* char ',' <*> elfWork
    elfWork = ElfWork <$> number <* char '-' <*> number
    number  = read <$> many digit
