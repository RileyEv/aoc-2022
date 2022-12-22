{-# LANGUAGE BlockArguments, DeriveAnyClass, DeriveGeneric, DerivingStrategies, ConstraintKinds, DataKinds, TypeFamilies, TypeOperators, GADTs, RankNTypes #-}
{-# OPTIONS_GHC -fplugin GHC.TypeLits.Normalise #-}

module Day15
    ( run
    , task1
    , task2
    , reader
    ) where
import           Common                         ( inputFileName
                                                , runTasks
                                                )
import           Data.Holmes
import           Data.Proxy
import qualified Data.Set                      as S
import           GHC.Natural
import           GHC.TypeNats
import           Text.Megaparsec
import           Text.Megaparsec.Char

data Observation = Observation !Pair !Pair
    deriving Show
type Pair = (Int, Int)

intervals :: Int -> [Observation] -> [Pair]
intervals line = foldr f []
  where
    f :: Observation -> [Pair] -> [Pair]
    f (Observation (sX, sY) (bX, bY)) ps
        | let d = abs (sX - bX) + abs (sY - bY) - abs (sY - line), d >= 0
        = (sX - d, sX + d) : ps
        | otherwise
        = ps

-- x values that are on the line 200000
thingsOnLine :: Pair -> [Pair] -> S.Set Int
thingsOnLine bounds is = S.fromList (concatMap f is)
  where
    f :: Pair -> [Int]
    f (xMin, xMax) = [(max xMin (fst bounds)) .. (min xMax (snd bounds))]

isBeaconOnLine :: Int -> Observation -> Bool
isBeaconOnLine line (Observation _ (_, y)) = y == line

beaconXValue :: Observation -> Int
beaconXValue (Observation _ (x, _)) = x

tuningFrequency :: Pair -> Int
tuningFrequency (x, y) = x * 4000000 + y


doTask1 :: Pair -> Int -> [Observation] -> IO (S.Set Int)
doTask1 bounds line obs = do
    let theIntervals  = intervals line obs
        xValuesOnLine = thingsOnLine bounds theIntervals
        beaconsOnLine =
            (S.fromList . map beaconXValue . filter (isBeaconOnLine line)) obs

        cannotContainBeacon = xValuesOnLine `S.difference` beaconsOnLine
    return cannotContainBeacon

miniTask1 :: Pair -> Int -> [Observation] -> IO (S.Set Int)
miniTask1 bounds line obs = do
    let theIntervals  = intervals line obs
        xValuesOnLine = thingsOnLine bounds theIntervals

    return xValuesOnLine

task1 :: Int -> [Observation] -> IO (Maybe Int)
task1 line obs = do
    -- print obs
    cannotContainBeacon <- doTask1 (-100000000, 100000000) line obs

    return (Just (S.size cannotContainBeacon))


-- doTask2 :: Pair -> [Observation] -> IO (Maybe [Defined Int])
-- doTask2 (xMax, _) obs = do
--     let guesses = 2 `from` [3000000 .. xMax]
--     guesses `satisfying` \[x, y] -> and' [ constraint ob x y | ob <- obs ]


-- constraint
--     :: ( OrdC f Int
--        , c1 Int
--        , Lifting f c1
--        , OrdR f
--        , c2 Int
--        , Lifting f c2
--        , AbsR (f Int)
--        , MonadCell m
--        , SumR (f Int)
--        )
--     => Observation
--     -> Prop m (f Int)
--     -> Prop m (f Int)
--     -> Prop m (f Bool)
-- constraint (Observation (sX, sY) (bX, bY)) x y =
--     let m = abs' (lift sX .- lift bX) .+ abs' (lift sY .- lift bY)
--     in  abs' (lift sX .- x) .+ abs' (lift sY .- y) .> m

doTask2 :: Pair -> [Observation] -> IO Pair
doTask2 (maxX, -1) obs = error "Couldnt find a single cell?"
doTask2 (maxX, y ) obs = do
    -- print y
    rowCannotBeBeacon <- miniTask1 (0, maxX) y obs
    -- print rowCannotBeBeacon
    if (S.size rowCannotBeBeacon == maxX + 1)
        then doTask2 (maxX, y - 1) obs
        else
            let [missingValue] =
                    S.toList
                        (S.fromList [0 .. maxX] `S.difference` rowCannotBeBeacon
                        )
            in  return (missingValue, y)

task2 :: (Int, Int) -> [Observation] -> IO (Maybe Int)
task2 bounds obs = do
    loc <- doTask2 bounds obs
    -- print loc

    return (Just (tuningFrequency loc))
-- Does not need to be read, this is just some way to manipulate each line of the input file
run = runTasks (inputFileName 'run)
               reader
               (task1 2000000)
               (task2 (4000000, 4000000))



type Parser = Parsec () String
reader :: String -> [Observation]
reader str = case parse observations "inputs/day15.txt" str of
    Left  parseError -> error (show parseError)
    Right y          -> y
  where
    observations :: Parser [Observation]
    observations = sepEndBy1 observation newline

    observation :: Parser Observation
    observation =
        Observation
            <$  string "Sensor at "
            <*> pair
            <*  string ": closest beacon is at "
            <*> pair

    pair :: Parser Pair
    pair =
        (,)
            <$  string "x="
            <*> maybeSignedNumber
            <*  string ", y="
            <*> maybeSignedNumber

    sign :: Parser Int
    sign = (-1) <$ char '-' <|> 1 <$ string ""

    maybeSignedNumber :: Parser Int
    maybeSignedNumber = (*) <$> sign <*> number

    number :: Parser Int
    number = read <$> some digitChar

-- * I have gone mad :)


-- data Nat = Succ Nat | Zero deriving (Show, Eq)

-- data SNat (n :: Nat) where
--   SZero ::SNat Zero
--   SSucc ::SNat n -> SNat (Succ n)


-- snatSize :: SNat n -> Int
-- snatSize SZero     = 0
-- snatSize (SSucc n) = 1 + snatSize n

-- instance Show (SNat n) where


-- type N0 = Zero
-- type N1 = Succ N0
-- type N2 = Succ N1
-- type N3 = Succ N2
-- type N4 = Succ N3
-- type N5 = Succ N4


-- type family (a :: Nat) := (b :: Nat) :: Bool where
--   Succ n := Zero = False
--   Zero := Zero = True
--   Zero := Succ m = False
--   Succ n := Succ m = n := m

-- type family (a :: Nat) :< (b :: Nat) :: Bool where
--   Succ n :< Zero = False
--   Zero :< Zero = False
--   Zero :< Succ m = True
--   Succ n :< Succ m = n :< m

-- type family (a :: Nat) :> (b :: Nat) :: Bool where
--   Succ n :> Zero = True
--   Zero :> Zero = False
--   Zero :> Succ m = False
--   Succ n :> Succ m = n :> m

-- class IsNat (n :: Nat) where
--   nat :: SNat n

-- instance IsNat Zero where
--     nat = SZero

-- instance IsNat n => IsNat (Succ n) where
--     nat = SSucc nat


-- thingy' :: Int
-- thingy' = snatSize thingy

-- type family And
