{-# LANGUAGE NamedFieldPuns, RecordWildCards #-}

module Day19
    ( run
    , task1
    , task2
    , reader
    ) where
import           Common                         ( inputFileName
                                                , runTasks
                                                )
import           Control.Monad                  ( guard
                                                , when
                                                )
import           Control.Monad.State            ( MonadState(get)
                                                , evalState
                                                , modify
                                                )
import           Data.HashMap.Strict            ( (!)
                                                , HashMap
                                                )
import qualified Data.HashMap.Strict           as M
import           Data.Hashable                  ( Hashable(hashWithSalt) )
import qualified Data.List                     as L
import           Data.Maybe                     ( mapMaybe )
import           Data.Traversable               ( for )
import           Text.Parsec                    ( Parsec
                                                , char
                                                , choice
                                                , digit
                                                , many1
                                                , parse
                                                , sepBy
                                                , spaces
                                                , string
                                                , try
                                                )
import           Text.Parsec.String             ( Parser )

data Resource
  = Ore
  | Clay
  | Obsidian
  | Geode
  deriving (Bounded, Enum, Eq, Ord)

instance Show Resource where
    show Ore      = "ore"
    show Clay     = "clay"
    show Obsidian = "obsidian"
    show Geode    = "geode"

instance Hashable Resource where
    hashWithSalt s = hashWithSalt s . fromEnum

type Input = [Blueprint]

type Blueprint = HashMap Resource Cost
type Cost = HashMap Resource Int
type Robots = HashMap Resource Int
type Stock = HashMap Resource Int




-- build

pay :: Stock -> Cost -> Stock
pay = M.unionWith (-)

collect :: Stock -> Robots -> Stock
collect = M.unionWith (+)

maxRobots :: Blueprint -> HashMap Resource Int
maxRobots = L.foldl1' (M.unionWith max) . M.elems

maxGeodes :: Int -> Blueprint -> Int
maxGeodes totalTime bp = flip evalState 0
    $ go totalTime (M.insert Ore 1 none) none
  where
    triangular n = div (n * (n + 1)) 2
    maxNeeded = maxRobots bp
    recipes   = M.toList bp
    go timeLeft robots bag = do
        let delays = flip Data.Maybe.mapMaybe recipes $ \(target, cost) -> do
                waitTimes <- flip
                    M.traverseWithKey
                    cost
                    (\resource amountNeeded -> case robots ! resource of
                        0 -> Nothing
                        r -> Just $ div
                            (max 0 (amountNeeded - bag ! resource) + r - 1)
                            r
                    )
                let delay = maximum waitTimes + 1
                guard $ timeLeft > delay
                when (target /= Geode)
                    $ guard
                    $ (maxNeeded ! target - robots ! target)
                    * timeLeft
                    > bag
                    ! target
                pure (target, cost, delay)
            justWait = timeLeft * robots ! Geode + bag ! Geode
        bestSoFar <- get
        let upperBound = justWait + triangular (timeLeft - 1)
        if bestSoFar >= upperBound
            then pure 0
            else do
                paths <- for
                    delays
                    (\(target, cost, delay) -> do
                        go (timeLeft - delay)
                           (M.insertWith (+) target 1 robots)
                           (pay (collect bag $ fmap (delay *) robots) cost)
                    )
                let r = maximum $ justWait : paths
                modify $ max r
                pure r



task1 :: [Blueprint] -> IO (Maybe Int)
task1 bps =
    return (Just ((sum . zipWith (*) [1 ..] . Prelude.map (maxGeodes 24)) bps))

task2 :: [Blueprint] -> IO (Maybe Int)
task2 bps = return (Just ((product . Prelude.map (maxGeodes 32) . take 3) bps))

-- Does not need to be read, this is just some way to manipulate each line of the input file
run = runTasks (inputFileName 'run) reader task1 task2


none :: HashMap Resource Int
none = M.fromList [ (r, 0) | r <- enumerate ]

enumerate :: (Bounded a, Enum a) => [a]
enumerate = [minBound .. maxBound]

(...) :: (c -> d) -> (a -> b -> c) -> a -> b -> d
(...) = (.) (.) (.)
infixr 8 ...

parseWith :: Parser a -> String -> a
parseWith = either (error . show) id ... flip parse ""
symbol :: String -> Parser String
symbol = lexeme . try . string
lexeme :: Parsec String u a -> Parsec String u a
lexeme p = p <* spaces
number :: Parser Int
number = lexeme
    $ choice [char '-' *> fmap negate digits, char '+' *> digits, digits]
    where digits = read <$> many1 digit
tryAll :: [Parser a] -> Parser a
tryAll = choice . Prelude.map try


reader :: String -> Input
reader = parseWith $ many1 blueprint
  where
    blueprint = do
        _ <- symbol "Blueprint"
        _ <- number
        _ <- symbol ":"
        M.fromList <$> many1 robot
    robot = do
        _        <- symbol "Each"
        target   <- resource
        _        <- symbol "robot costs"
        costList <- fmap M.fromList $ cost `sepBy` symbol "and"
        _        <- symbol "."
        pure (target, costList)
    cost = do
        n <- number
        r <- resource
        pure (r, n)
    resource = tryAll
        [ Ore <$ symbol "ore"
        , Clay <$ symbol "clay"
        , Obsidian <$ symbol "obsidian"
        , Geode <$ symbol "geode"
        ]
