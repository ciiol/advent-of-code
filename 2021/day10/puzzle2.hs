#! /usr/bin/env nix-shell
#! nix-shell -i runhaskell -p

-- https://adventofcode.com/2021/day/10#part2
-- The navigation subsystem syntax is made of several lines containing chunks.
-- There are one or more chunks on each line, and chunks contain zero or more other
-- chunks. Adjacent chunks are not separated by any delimiter; if one chunk stops,
-- the next chunk (if any) can immediately start. Every chunk must open and close
-- with one of four legal pairs of matching characters

-- Incomplete lines don't have any incorrect characters - instead, they're missing
-- some closing characters at the end of the line. To repair the navigation subsystem,
-- you just need to figure out the sequence of closing characters that complete all
-- open chunks in the line.

-- Find the completion string for each incomplete line, score the completion
-- strings, and sort the scores. What is the middle score?

import Data.Maybe (mapMaybe)
import Data.List (sort, foldl')
import System.Exit (exitFailure)
import Text.Parsec.Language (emptyDef)
import Text.ParserCombinators.Parsec (Parser, char, parse, many1, oneOf, sepEndBy1)

lineParser :: Parser [Char]
lineParser = many1 $ oneOf "<>[](){}"

findCompletion :: [Char] -> Maybe [Char]
findCompletion = find' []
  where
    find' stack [] = Just stack
    find' stack (x:xs) | isOpen x = find' (matching x : stack) xs
    find' [] (_x:_) = Nothing
    find' (s:stack) (x:xs) | s == x = find' stack xs
    find' (s:_) (x:_) | s /= x = Nothing
    find' _ _ = error "oops"

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

score :: [Char] -> Integer
score = foldl' (\ a x -> a * 5 + score' x) 0
  where
    score' '>' = 4
    score' '}' = 3
    score' ']' = 2
    score' ')' = 1
    score' _ = error "oops"

median :: (Ord a) => [a] -> a
median list = sort list !! (length list `div` 2)

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
  print $ median $ map score $ mapMaybe findCompletion input
