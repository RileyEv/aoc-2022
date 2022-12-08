module Day7
    ( run
    , task1
    , task2
    , reader
    ) where
import           Common                         ( inputFileName
                                                , runTasks
                                                )
import           Data.List                      ( foldl' )
import qualified Data.Map                      as M
import           Text.Megaparsec
import           Text.Megaparsec.Char

data Object = File Int String | Directory String deriving Show

data Command = Cd Direction | Ls [Object] deriving Show
data Direction = Up | Root | Down String deriving Show



insertObjects :: [Object] -> FileSystem -> FileSystem
insertObjects objects fs = foldr f fs objects
  where
    f :: Object -> FileSystem -> FileSystem
    f (File size name) (FileSystem fs) =
        FileSystem (M.insert name (Right (FSFile size)) fs)
    f (Directory name) (FileSystem fs) =
        FileSystem (M.insert name (Left (FileSystem M.empty)) fs)
    -- where newSubObjects = [ objectToFs ob | ob <- objects ] ++ subObjects

getChild :: String -> FileSystem -> Maybe (Either FileSystem FSFile)
getChild name (FileSystem fs) = fs M.!? name


type Path = [String]
newtype FSFile = FSFile Int deriving Show
newtype FileSystem = FileSystem (M.Map String (Either FileSystem FSFile)) deriving Show

dropLast :: [a] -> [a]
dropLast []       = []
dropLast [_x    ] = []
dropLast (x : xs) = x : dropLast xs

update :: String -> FileSystem -> FileSystem -> FileSystem
update name fs (FileSystem map) = FileSystem (M.insert name (Left fs) map)

executeCommand :: Command -> (FileSystem, Path) -> (FileSystem, Path)
executeCommand (Ls objects) (fs, []) = (insertObjects objects fs, [])
executeCommand cmd@(Ls _objects) (fs, path@(p : ps)) =
    let childDir = case getChild p fs of
            Just (Left  fs') -> fs'
            Just (Right _f ) -> error "cannot ls a file"
            Nothing          -> error (show (cmd, fs, path))

        (newFs, _) = executeCommand cmd (childDir, ps)
    in  (update p newFs fs, path)
executeCommand (Cd Root    ) (fs, _path) = (fs, [])
executeCommand (Cd Up      ) (fs, path ) = (fs, dropLast path)
executeCommand (Cd (Down p)) (fs, path ) = (fs, path ++ [p])

emptyFS :: FileSystem
emptyFS = FileSystem M.empty

buildFS :: [Command] -> (FileSystem, Path)
buildFS = foldl' (flip executeCommand) (emptyFS, [])

doTask1 :: FileSystem -> (Int, Int)
doTask1 (FileSystem map) = M.foldr thing (0, 0) map
  where
    -- ( size of object, sum of all dirs at most 100000 )
    thing :: Either FileSystem FSFile -> (Int, Int) -> (Int, Int)
    thing (Left fs) (x, y) =
        let (dirSize, totalInDir) = doTask1 fs
        in  ( dirSize + x
            , y + totalInDir + if dirSize <= 100000 then dirSize else 0
            )
    thing (Right (FSFile size)) (x, y) = (size + x, y)


doTask2 :: Int -> FileSystem -> (Int, Int)
doTask2 requiredDeletion (FileSystem map) = M.foldr thing (0, 70000000) map
  where
    -- ( size of object, current minimum deletion )
    thing :: Either FileSystem FSFile -> (Int, Int) -> (Int, Int)
    thing (Left fs) (x, y) =
        let (dirSize, currentDeletion) = doTask2 requiredDeletion fs
        in  ( dirSize + x
            , if dirSize >= requiredDeletion
                then min dirSize (min currentDeletion y)
                else min currentDeletion y
            )
    thing (Right (FSFile size)) (x, y) = (size + x, y)


task1 :: [Command] -> IO (Maybe Int)
task1 cs = do
    let (fs, _)         = buildFS cs
    let (_size, result) = doTask1 fs
    return (Just result)

task2 :: [Command] -> IO (Maybe Int)
task2 cs = do
    let (fs, _)        = buildFS cs
    let (usedSpace, _) = doTask1 fs

    let maxSpace          = 70000000
        requiredFreeSpace = 30000000
        requiredDeletion  = requiredFreeSpace - (maxSpace - usedSpace)

    let (_, result) = doTask2 requiredDeletion fs

    return (Just result)

-- Does not need to be read, this is just some way to manipulate each line of the input file
run = runTasks (inputFileName 'run) reader task1 task2

type Parser = Parsec () String
reader :: String -> [Command]
reader str = case parse commands "inputs/day5.txt" str of
    Left  parseError -> error (show parseError)
    Right y          -> y
  where
    commands = some command
    command :: Parser Command
    command = try cd <|> ls
    cd :: Parser Command
    cd = Cd <$ string "$ cd " <*> direction <* newline
    direction :: Parser Direction
    direction = Up <$ string ".." <|> Root <$ string "/" <|> Down <$> dirName
    ls :: Parser Command
    ls = Ls <$ string "$ ls" <* newline <*> objects
    objects :: Parser [Object]
    objects = many ((try file <|> dir) <* newline)
    dir :: Parser Object
    dir     = Directory <$ string "dir " <*> dirName
    dirName = some (oneOf ['a' .. 'z'])
    file :: Parser Object
    file     = File <$> number <* char ' ' <*> fileName
    fileName = some (oneOf ('.' : ['a' .. 'z']))
    number :: Parser Int
    number = read <$> many digitChar
