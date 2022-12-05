module Common where

import           Control.Monad                  ( (<=<) )
import           Data.Char                      ( toLower )
import           Language.Haskell.TH            ( Name
                                                , nameModule
                                                )

data Task = Task1 | Task2 | All


inputFile :: FilePath -> IO String
inputFile = readFile

inputFileLines :: FilePath -> IO [String]
inputFileLines fname = do
  f <- readFile fname
  return (lines f)

inputFileLinesMap :: (String -> b) -> FilePath -> IO [b]
inputFileLinesMap mapFunc = mapM (return . mapFunc) <=< inputFileLines


runTasksLines
  :: (Show b, Show c)
  => FilePath
  -> (String -> a)
  -> ([a] -> IO (Maybe b))
  -> ([a] -> IO (Maybe c))
  -> Task
  -> IO ()
runTasksLines fname mapFunc t1 t2 taskOption = case taskOption of
  Task1 -> task1
  Task2 -> task2
  All   -> task1 >> task2
 where
  input' = inputFileLinesMap mapFunc fname
  task1  = do
    input    <- input'
    task1Res <- t1 input
    print ("Task1: " ++ show task1Res)
  task2 = do
    input    <- input'
    task2Res <- t2 input
    print ("Task2: " ++ show task2Res)

runTasks
  :: (Show b, Show c)
  => FilePath
  -> (String -> a)
  -> (a -> IO (Maybe b))
  -> (a -> IO (Maybe c))
  -> Task
  -> IO ()
runTasks fname mapFunc t1 t2 taskOption = case taskOption of
  Task1 -> task1
  Task2 -> task2
  All   -> task1 >> task2
 where
  input' = mapFunc <$> inputFile fname
  task1  = do
    print "hi"
    input    <- input'
    task1Res <- t1 input
    print ("Task1: " ++ show task1Res)
  task2 = do
    input    <- input'
    task2Res <- t2 input
    print ("Task2: " ++ show task2Res)

inputFileName :: Name -> String
inputFileName name = case nameModule name of
  (Just moduleName) ->
    "inputs/" ++ map toLower (head (split '.' moduleName)) ++ ".txt"
  Nothing -> error "Ahhhhh there's not module name?!?!!?!? :o"

split :: Char -> String -> [String]
split c s = case rest of
  []        -> [chunk]
  _ : rest' -> chunk : split c rest'
  where (chunk, rest) = break (== c) s
