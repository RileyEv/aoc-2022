{-# LANGUAGE RecordWildCards, NamedFieldPuns #-}
module Day11
    ( run
    , task1
    , task2
    , reader
    , Monkey(..)
    ) where
import           Common                         ( inputFileName
                                                , runTasks
                                                )
import           Data.List                      ( sortBy )
import           Data.Map                      as M
import           Data.Vector                   as V
data Monkey = Monkey
    { items     :: ![Int]
    , operation :: !(Int -> Int)
    , divisor   :: !Int
    , ifTrue    :: !Int
    , ifFalse   :: !Int
    , itemsSeen :: !Int
    , index     :: !Int
    }

instance Show Monkey where
    show Monkey { items, itemsSeen } = show (items, itemsSeen)

applyMovingItems :: M.Map Int [Int] -> V.Vector Monkey -> V.Vector Monkey
applyMovingItems itemsToUpdate ms' = foldrWithKey f ms' itemsToUpdate
  where
    f :: Int -> [Int] -> V.Vector Monkey -> V.Vector Monkey
    f idx newMonkeyItems ms =
        let currentMonkey = ms V.! idx
            newMonkey     = currentMonkey
                { items = items currentMonkey Prelude.++ newMonkeyItems
                }
        in  (ms V.// [(idx, newMonkey)])

doMod :: V.Vector Monkey -> V.Vector Monkey
doMod ms = let bigMod = V.product (V.map divisor ms) in V.map (f bigMod) ms
  where
    f bigMod m@Monkey { items } =
        m { items = Prelude.map (`mod` bigMod) items }

doRound :: Int -> V.Vector Monkey -> IO (V.Vector Monkey)
doRound worryLevelDivisor ms = do
    (monkeys, movingItems) <- V.foldl (flip f) (return (V.empty, M.empty)) ms
    -- print monkeys
    -- print movingItems
    return $ (doMod . applyMovingItems movingItems) monkeys
  where
    f
        :: Monkey
        -> IO (V.Vector Monkey, M.Map Int [Int])
        -> IO (V.Vector Monkey, M.Map Int [Int])
    f m@Monkey {..} v = do
        (ms', itemsToMove) <- v
        -- print (index, m, ms', itemsToMove)
        let maybeAlreadyThrownToMe            = itemsToMove M.!? index
            (alreadyThrownToMe, itemsToMove') = case maybeAlreadyThrownToMe of
                Just x  -> (x, M.delete index itemsToMove)
                Nothing -> ([], itemsToMove)
            (updatedItems, monkeysItemsToMove) = Prelude.foldr
                (f2 m)
                ([], M.empty)
                (items Prelude.++ alreadyThrownToMe)
            updatedMonkey = m
                { items     = updatedItems
                , itemsSeen = itemsSeen
                              + Prelude.length items
                              + Prelude.length alreadyThrownToMe
                }
            updatedItemsToMove =
                M.unionWith (Prelude.++) itemsToMove' monkeysItemsToMove
        let x = (ms' `V.snoc` updatedMonkey, updatedItemsToMove)
        -- print (index, x)
        return x
    f2 :: Monkey -> Int -> ([Int], M.Map Int [Int]) -> ([Int], M.Map Int [Int])
    f2 Monkey {..} item' (keptItems, throwAwayItems) =
        let worryLevel  = operation item'
            worryLevel' = worryLevel `div` worryLevelDivisor
            isTrue'     = worryLevel' `mod` divisor == 0
            newMonkey   = if isTrue' then ifTrue else ifFalse
        in  ( keptItems
            , M.insertWith (Prelude.++) newMonkey [worryLevel'] throwAwayItems
            )

nRounds :: Int -> Int -> V.Vector Monkey -> IO (V.Vector Monkey)
nRounds 0 _                 ms = return ms
nRounds n worryLevelDivisor ms = do
    -- print ("Round: " Prelude.++ show n)
    round <- doRound worryLevelDivisor ms
    -- print round
    nRounds (n - 1) worryLevelDivisor round

activitySort :: [Monkey] -> [Monkey]
activitySort = sortBy sortF
  where
    sortF :: Monkey -> Monkey -> Ordering
    sortF m1 m2 = compare (itemsSeen m1) (itemsSeen m2)


task1 :: [Monkey] -> IO (Maybe Int)
task1 ms = do
    round20 <- nRounds 20 3 (V.fromList ms)
    let listMonkeys    = V.toList round20
        sortedMonkeys  = activitySort listMonkeys
        (m1 : m2 : []) = Prelude.take 2 (Prelude.reverse sortedMonkeys)

    -- print round20

    return (Just (itemsSeen m1 * itemsSeen m2))

task2 :: [Monkey] -> IO (Maybe Int)
task2 ms = do
    round20 <- nRounds 10000 1 (V.fromList ms)
    let listMonkeys    = V.toList round20
        sortedMonkeys  = activitySort listMonkeys
        (m1 : m2 : []) = Prelude.take 2 (Prelude.reverse sortedMonkeys)

    -- print round20

    return (Just (itemsSeen m1 * itemsSeen m2))

-- Does not need to be read, this is just some way to manipulate each line of the input file
run = runTasks (inputFileName 'run) reader task1 task2

reader :: String -> [Monkey]
reader _ =
    [ Monkey [62, 92, 50, 63, 62, 93, 73, 50] (* 7)         2  7 1 0 0
    , Monkey [51, 97, 74, 84, 99]             (+ 3)         7  2 4 0 1
    , Monkey [98, 86, 62, 76, 51, 81, 95]     (+ 4)         13 5 4 0 2
    , Monkey [53, 95, 50, 85, 83, 72]         (+ 5)         19 6 0 0 3
    , Monkey [59, 60, 63, 71]                 (* 5)         11 5 3 0 4
    , Monkey [92, 65]                         (\x -> x * x) 5  6 3 0 5
    , Monkey [78]                             (+ 8)         3  0 7 0 6
    , Monkey [84, 93, 54]                     (+ 1)         17 2 1 0 7
    ]
