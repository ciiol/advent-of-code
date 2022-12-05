#! /usr/bin/env nix-shell
#! nix-shell -i runhaskell -p

-- https://adventofcode.com/2022/day/5
-- Your puzzle input is a drawing of the starting stacks of crates
-- and the rearrangement procedure.

-- After the rearrangement procedure completes, what crate ends up on top of each stack?

import Text.ParserCombinators.Parsec (Parser, sepBy1, sepEndBy1, try, alphaNum,
                                      char, parse, string, (<|>), digit, many1)
import System.Exit (exitFailure)
import Data.List (transpose, foldl')
import Data.Maybe (catMaybes, fromMaybe)
import Data.Map.Strict as M (Map, fromList, adjust, lookup, elems)

type Crate = Char
type Stack = [Crate]
type Index = Int
type Cargo = M.Map Index Stack
data Instruction = Move Int Index Index deriving (Show)

cargoParser :: Parser Cargo
cargoParser = makeCargo <$> sepEndBy1 (try line) (char '\n') <*> indexesParser
  where
    line = sepBy1 (parseCrate <|> parseNoCrate) (char ' ')

indexesParser :: Parser [Index]
indexesParser = char ' ' *> sepBy1 indexParser (try $ string "   ") <* string " \n"

indexParser :: Parser Index
indexParser = read <$> many1 digit

parseCrate :: Parser (Maybe Crate)
parseCrate = Just <$> (char '[' *> alphaNum <* char ']')

parseNoCrate :: Parser (Maybe Crate)
parseNoCrate = do
  _ <- string "   "
  return Nothing

makeCargo :: [[Maybe Crate]] -> [Index] -> Cargo
makeCargo lines indexes = M.fromList $ zip indexes stacks 
  where
    stacks = map catMaybes $ transpose lines

instructionParser :: Parser Instruction
instructionParser = do
  _ <- string "move "
  num <- read <$> many1 digit
  _ <- string " from "
  from <- indexParser
  _ <- string " to "
  to <- indexParser
  return $ Move num from to

instructionsParser :: Parser [Instruction]
instructionsParser = sepEndBy1 instructionParser (char '\n')

inputParser :: Parser (Cargo, [Instruction])
inputParser = (,) <$> cargoParser <*> (char '\n' *> instructionsParser)

apply :: Cargo -> Instruction -> Cargo
apply cargo (Move 0 _ _) = cargo
apply cargo (Move num from to) = apply newCargo (Move (num - 1) from to)
  where
    elem = head stackFrom
    stackFrom = fromMaybe (error "no from") $ M.lookup from cargo
    newCargo = M.adjust (elem :) to $ M.adjust tail from cargo

parseInput :: Parser a -> IO a
parseInput parser = parseInput' parser >>= either report return
  where
    report err = do
      putStrLn $ "Error: " ++ show err
      exitFailure
    parseInput' p = do parse p "(stdin)" <$> getContents

main :: IO ()
main = do
  (cargo, instructions) <- parseInput inputParser
  print $ map head $ M.elems $ foldl' apply cargo instructions
