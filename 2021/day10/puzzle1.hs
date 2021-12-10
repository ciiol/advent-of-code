#! /usr/bin/env nix-shell
#! nix-shell -i runhaskell -p

-- https://adventofcode.com/2021/day/10
-- The navigation subsystem syntax is made of several lines containing chunks.
-- There are one or more chunks on each line, and chunks contain zero or more other
-- chunks. Adjacent chunks are not separated by any delimiter; if one chunk stops,
-- the next chunk (if any) can immediately start. Every chunk must open and close
-- with one of four legal pairs of matching characters

-- Some of the lines aren't corrupted, just incomplete; you can ignore these lines for now.

-- Find the first illegal character in each corrupted line of the navigation subsystem.
-- What is the total syntax error score for those errors?

import Data.Maybe (mapMaybe)
import System.Exit (exitFailure)
import Text.Parsec.Language (emptyDef)
import Text.ParserCombinators.Parsec (Parser, char, parse, many1, oneOf, sepEndBy1)

lineParser :: Parser [Char]
lineParser = many1 $ oneOf "<>[](){}"

findError :: [Char] -> Maybe Char
findError = findError' []
  where
    findError' _stack [] = Nothing
    findError' stack (x:xs) | isOpen x = findError' (matching x : stack) xs
    findError' [] (x:_) = Just x
    findError' (s:stack) (x:xs) | s == x = findError' stack xs
    findError' (s:_) (x:_) | s /= x = Just x
    findError' _ _ = error "oops"

isOpen :: Char -> Bool 
isOpen '<' = True
isOpen '(' = True
isOpen '[' = True
isOpen '{' = True
isOpen _ = False

matching :: Char -> Char
matching '<' = '>'
matching '(' = ')'
matching '[' = ']'
matching '{' = '}'
matching _ = error "unexpected match"

score :: Char -> Integer
score '>' = 25137
score '}' = 1197
score ']' = 57
score ')' = 3
score _ = error "oops"

parseInput :: Parser a -> IO a
parseInput parser = parseInput' parser >>= either report return
  where
    report err = do
      putStrLn $ "Error: " ++ show err
      exitFailure
    parseInput' p = do parse p "(stdin)" <$> getContents

main :: IO ()
main = do
  input <- parseInput $ sepEndBy1 lineParser $ char '\n'
  print $ sum $ map score $ mapMaybe findError input
