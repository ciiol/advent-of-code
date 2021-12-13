#! /usr/bin/env nix-shell
#! nix-shell -i runhaskell -p

-- https://adventofcode.com/2021/day/13
-- To your surprise, you manage to find the manual; as you go to open it, page 1 falls out.
-- It's a large sheet of transparent paper! The transparent paper is marked with random dots
-- and includes instructions on how to fold it up (your puzzle input).

-- How many dots are visible after completing just the first fold instruction on your transparent paper?

import Data.Functor (($>))
import qualified Data.Set as S
import System.Exit (exitFailure)
import Text.Parsec.Language (emptyDef)
import Text.ParserCombinators.Parsec (Parser, char, many1, parse, string, (<|>))
import qualified Text.ParserCombinators.Parsec.Token as P

data Node = Small String | Large String deriving (Eq, Ord, Show)

type Coord = (Integer, Integer)

type Page = S.Set Coord

data Folding = ByX Integer | ByY Integer deriving (Show)

lexer = P.makeTokenParser emptyDef

inputParser :: Parser (Page, [Folding])
inputParser = (,) <$> (makePage <$> many1 coordParser) <*> many1 foldingParser

coordParser :: Parser Coord
coordParser = do
  x <- P.natural lexer
  _ <- char ','
  y <- P.natural lexer
  return (x, y)

foldingParser :: Parser Folding
foldingParser = do
  _ <- string "fold along "
  by <- string "x=" $> ByX <|> string "y=" $> ByY
  value <- P.natural lexer
  return $ by value

makePage :: [Coord] -> Page
makePage = S.fromList

foldPage :: Page -> Folding -> Page
foldPage page folding = S.map (applyFold folding) page
  where
    applyFold (ByX a) p@(x, _y) | x <= a = p
    applyFold (ByX a) (x, y) | x > a = (a * 2 - x, y)
    applyFold (ByY a) p@(_x, y) | y <= a = p
    applyFold (ByY a) (x, y) | y > a = (x, a * 2 - y)
    applyFold _ _ = error "oops"

parseInput :: Parser a -> IO a
parseInput parser = parseInput' parser >>= either report return
  where
    report err = do
      putStrLn $ "Error: " ++ show err
      exitFailure
    parseInput' p = do parse p "(stdin)" <$> getContents

main :: IO ()
main = do
  (page, foldings) <- parseInput inputParser
  print $ S.size $ foldPage page $ head foldings
