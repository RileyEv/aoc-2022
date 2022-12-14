module Day13
    ( run
    , task1
    , task2
    , reader
    , RList(..)
    ) where
import           Common                         ( inputFileName
                                                , runTasks
                                                )

import           Data.List                      ( elemIndex
                                                , findIndices
                                                , sortBy
                                                )
import           Data.Maybe                     ( fromJust )
import           Text.Megaparsec
import           Text.Megaparsec.Char

data Pair = Pair !RList !RList
    deriving Show
newtype RList = RList [Item] deriving (Show, Eq)
type Item = Either Int RList



doTask1SinglePair :: Pair -> Bool
doTask1SinglePair (Pair l r) = case go l r of
    Just x  -> x
    Nothing -> error "hmm how did we get here?"

go :: RList -> RList -> Maybe Bool
go (RList (Left x : xss)) (RList (Left y : yss))
    | x < y  = Just True
    | x > y  = Just False
    | x == y = go (RList xss) (RList yss)
go (RList []              ) (RList []              ) = Nothing -- Not sure?? doesnt seem to be defined?
go (RList (Right xs : xss)) (RList (Right ys : yss)) = case go xs ys of
    Just b  -> Just b
    Nothing -> go (RList xss) (RList yss)
go (RList []         ) (RList (_y : _yss)) = Just True
go (RList (_x : _xss)) (RList []         ) = Just False
go (RList (Right xs : xss)) (RList (Left y : yss)) =
    case go xs (RList [Left y]) of
        Just b  -> Just b
        Nothing -> go (RList xss) (RList yss)
go (RList (Left x : xss)) (RList (Right ys : yss)) =
    case go (RList [Left x]) ys of
        Just b  -> Just b
        Nothing -> go (RList xss) (RList yss)


task1 :: [Pair] -> IO (Maybe Int)
task1 pairs = do
    let results = map doTask1SinglePair pairs
        indices = (map (+ 1) . findIndices id) results

    return (Just (sum indices))

flattenPairs :: [Pair] -> [RList]
flattenPairs []                = []
flattenPairs ((Pair l r) : ps) = l : r : flattenPairs ps

sorter :: RList -> RList -> Ordering
sorter l r = case go l r of
    Just True  -> LT
    Just False -> GT
    Nothing    -> EQ

task2 :: [Pair] -> IO (Maybe Int)
task2 pairs = do
    let flatPairs = flattenPairs pairs
        locaterPackets =
            [RList [Right (RList [Left 2])], RList [Right (RList [Left 6])]]
        lists       = locaterPackets ++ flatPairs
        sortedLists = sortBy sorter lists
        idxs =
            map ((+ 1) . fromJust . (`elemIndex` sortedLists)) locaterPackets

    return (Just (product idxs))

-- Does not need to be read, this is just some way to manipulate each line of the input file
run = runTasks (inputFileName 'run) reader task1 task2

type Parser = Parsec () String
reader :: String -> [Pair]
reader str = case parse pairs "inputs/day13.txt" str of
    Left  parseError -> error (show parseError)
    Right y          -> y
  where
    pairs :: Parser [Pair]
    pairs = sepEndBy1 pair (some newline)

    pair :: Parser Pair
    pair = Pair <$> rlist <* newline <*> rlist

    rlist :: Parser RList
    rlist = RList <$> between (char '[') (char ']') (sepBy item (char ','))

    item :: Parser (Either Int RList)
    item = Left <$> number <|> Right <$> rlist

    number :: Parser Int
    number = read <$> some digitChar
