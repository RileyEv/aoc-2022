{-# LANGUAGE TupleSections, Strict #-}
module Day20
    ( run
    , task1
    , task2
    , reader
    ) where
import           Common                         ( inputFileName
                                                , runTasksLines
                                                )
import           Data.Maybe                     ( fromJust )
import qualified Data.Vector.Unboxed           as V

newIndex :: Int -> Int -> Int -> Int
newIndex len currentIdx value = (currentIdx + value) `mod` len

-- O(n)
pop :: V.Unbox a => Int -> V.Vector a -> (a, V.Vector a)
pop idx v =
    let (h , t) = V.splitAt (idx + 1) v
        (h', x) = (fromJust . V.unsnoc) h
    in  (x, h' V.++ t)

-- O(n)
dropIn :: V.Unbox a => Int -> a -> V.Vector a -> V.Vector a
dropIn idx value v =
    let (h, t) = V.splitAt idx v
        h'     = V.snoc h value
    in  h' V.++ t

-- O(n)
moveX :: Int -> V.Vector (Int, Int) -> V.Vector (Int, Int)
moveX idx v =
    let (x@(_startIdx, value), v') = pop idx v
        idx'                       = newIndex (V.length v') idx value
    in  dropIn idx' x v'

-- O(n^2)
decrypt :: V.Vector (Int, Int) -> V.Vector (Int, Int)
decrypt xs = foldr f xs (reverse [0 .. V.length xs - 1])
  where
    -- O(n)
    f :: Int -> V.Vector (Int, Int) -> V.Vector (Int, Int)
    f idx v = let newIdx = findNextIdx idx v in moveX (fromJust newIdx) v

-- O(n)
findNextIdx :: Int -> V.Vector (Int, Int) -> Maybe Int
findNextIdx idx = V.findIndex ((idx ==) . fst)

getCoords :: V.Vector (Int, Int) -> [Int]
getCoords v =
    let items   = V.map snd v
        zeroIdx = fromJust (V.elemIndex 0 items)
    in  map ((items V.!) . (`mod` V.length items) . (zeroIdx +))
            [1000, 2000, 3000]

task1 :: [Int] -> IO (Maybe Int)
task1 xs = do
    let seq    = (V.indexed . V.fromList) xs
    let res    = decrypt seq

    let coords = getCoords res

    return (Just (sum coords))

task2 :: [Int] -> IO (Maybe Int)
task2 xs = do
    let seq      = (V.indexed . V.fromList) xs
        withKey  = V.map (\(idx, x) -> (idx, x * 811589153)) seq

        runs     = iterate decrypt withKey
        tenthRun = runs !! 10
    let coords = getCoords tenthRun

    return (Just (sum coords))

-- Does not need to be read, this is just some way to manipulate each line of the input file
run = runTasksLines (inputFileName 'run) reader task1 task2

reader :: String -> Int
reader = read
