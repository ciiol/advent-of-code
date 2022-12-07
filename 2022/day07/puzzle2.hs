#! /usr/bin/env nix-shell
#! nix-shell -i runhaskell -p

-- https://adventofcode.com/2022/day/7
-- You browse around the filesystem to assess the situation and save the resulting terminal
-- output (your puzzle input).

-- The total disk space available to the filesystem is 70000000. To run the update, you need unused space
-- of at least 30000000. You need to find a directory you can delete that will free up enough space to
-- run the update.

-- Find the smallest directory that, if deleted, would free up enough space on the filesystem to run
-- the update. What is the total size of that directory?

import Text.ParserCombinators.Parsec (Parser, parse, try, alphaNum, digit, many1, string, char,
                                      sepEndBy, (<|>), many1, oneOf)
import System.Exit (exitFailure)
import Data.Map.Strict as M (Map, toList, empty, union, insert, findWithDefault, elems)
import Data.List (foldl', minimumBy)
import Data.Function (on)

type Path = String
data DirectoryItem = Dir Path | FileDescription Integer Path deriving (Show, Eq)
data LogEntry = Cd Path | Ls [DirectoryItem] deriving (Show, Eq)

data FileTree = Directory (M.Map Path FileTree) | File Integer deriving (Show, Eq)

logParser :: Parser [LogEntry]
logParser = many1 (try cdParser <|> try lsParser)

cdParser :: Parser LogEntry
cdParser = do
  _ <- string "$ cd "
  path <- pathParser
  _ <- char '\n'
  return $ Cd path

lsParser :: Parser LogEntry
lsParser = do
  _ <- string "$ ls\n"
  content <- sepEndBy (try directoryItemParser) (char '\n')
  return $ Ls content

directoryItemParser :: Parser DirectoryItem
directoryItemParser = try dirParser <|> try fileParser

dirParser :: Parser DirectoryItem
dirParser = Dir <$> (string "dir " *> pathParser)

fileParser :: Parser DirectoryItem
fileParser = FileDescription <$> (read <$> many1 digit) <*> (char ' ' *> pathParser)

pathParser :: Parser String
pathParser = many1 (alphaNum <|> oneOf "./")

addToTree :: [Path] -> FileTree -> FileTree -> FileTree
addToTree [] f@(File size1) (File size2) | size1 == size2 = f
addToTree [] f@(File size1) (File size2) | size1 /= size2 = error "oops, wrong size"
addToTree _ (File _) _ = error $ "oops, file found"
addToTree [] (Directory _) (File _) = error "oops, empty path"
addToTree [] (Directory sub) (Directory new) = Directory $ M.union sub new
addToTree [name] (Directory sub) f@(File _) = Directory $ M.insert name f sub
addToTree (p:xp) (Directory sub) new = Directory $ M.insert p inner sub
  where
    inner = addToTree xp (M.findWithDefault (Directory M.empty) p sub) new

buildTree :: [LogEntry] -> FileTree
buildTree = snd . foldl' buildTree' ([], Directory M.empty)
  where
    buildTree' (path, tree) (Cd "..") = (tail path, tree)
    buildTree' (path, tree) (Cd newPath) = (newPath : path, tree)
    buildTree' (path, tree) (Ls content) = (path, foldl' (addToTree' path) tree content)
    addToTree' path tree (Dir name) = addToTree (reverse path) tree $ Directory M.empty
    addToTree' path tree (FileDescription size name) = addToTree (reverse $ name : path) tree $ File size

isDir :: FileTree -> Bool
isDir (Directory _) = True
isDir _ = False

treeSize :: FileTree -> Integer
treeSize (File size) = size
treeSize (Directory tree) = sum $ map treeSize $ M.elems tree

getDirs :: FileTree -> [(Path, FileTree)]
getDirs (File _) = []
getDirs (Directory tree) = filter (isDir . snd) $ M.toList tree

findDirectoriesSizes :: FileTree -> [([Path], Integer)]
findDirectoriesSizes = findDirectoriesSizes' []
  where
    findDirectoriesSizes' path tree = (path, treeSize tree) : concatMap (findDirectoriesSizes'' path) (getDirs tree)
    findDirectoriesSizes'' path (p, tree) = findDirectoriesSizes' (p : path) tree

findDirToRemove :: FileTree -> ([Path], Integer)
findDirToRemove tree = minimumBy (compare `on` snd) candidates
  where
    total = treeSize tree
    used = 70000000 - total
    need = 30000000 - used
    sizes = findDirectoriesSizes tree
    candidates = filter ((need <=) . snd) sizes

parseInput :: Parser a -> IO a
parseInput parser = parseInput' parser >>= either report return
  where
    report err = do
      putStrLn $ "Error: " ++ show err
      exitFailure
    parseInput' p = do parse p "(stdin)" <$> getContents

main :: IO ()
main = do
  log <- parseInput logParser
  print $ findDirToRemove $ buildTree $ tail log
