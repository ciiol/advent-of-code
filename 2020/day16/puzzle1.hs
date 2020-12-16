#! /usr/bin/env nix-shell
#! nix-shell -i runhaskell -p

-- https://adventofcode.com/2020/day/16
-- Unfortunately, you can't actually read the words on the ticket.
-- You can, however, read the numbers, and so you figure out the
-- fields these tickets must have and the valid ranges for values
-- in those fields.

-- You collect the rules for ticket fields, the numbers on
-- your ticket, and the numbers on other nearby tickets for
-- the same train service (via the airport security cameras)
-- together into a single document you can reference
-- (your puzzle input).

-- Consider the validity of the nearby tickets you scanned.
-- What is your ticket scanning error rate?

import qualified Text.ParserCombinators.Parsec.Token as P
import Text.ParserCombinators.Parsec (Parser, (<|>), parse, sepBy1, newline, manyTill, anyChar, char, try, many)
import Text.Parsec.Language (emptyDef)
import System.Exit (exitFailure)

data Rule = Rule {rName :: String, rRanges :: [Range]} deriving (Show, Eq, Ord)
data Range = Range {rLeft :: Integer, rRight :: Integer} deriving (Show, Eq, Ord)
data Input = Input {iRules :: [Rule], iYour :: Ticket, iNearby :: [Ticket]} deriving (Show, Eq, Ord)
type Ticket = [Integer]

rulesParser :: Parser [Rule]
rulesParser = manyTill ruleParser (try (P.symbol lexer "your ticket:"))

ruleParser :: Parser Rule
ruleParser = do
  name <- manyTill anyChar $ (char ':' <|> newline)
  _ <- P.whiteSpace lexer
  ranges <- rangesParser
  return $ Rule name ranges

rangesParser :: Parser [Range]
rangesParser = sepBy1 rangeParser (P.symbol lexer "or")

rangeParser :: Parser Range
rangeParser = do
  left <- P.natural lexer
  _ <- char '-'
  right <- P.natural lexer
  return $ Range left right

ticketParser :: Parser Ticket
ticketParser = sepBy1 (P.natural lexer) (P.comma lexer)

inputParser :: Parser Input
inputParser = do
  rules <- rulesParser
  your <- ticketParser
  _ <- (try (P.symbol lexer "nearby tickets:"))
  nearby <- many ticketParser
  return $ Input rules your nearby

lexer = P.makeTokenParser emptyDef

inRange :: Integer -> Range -> Bool
inRange x (Range left right) = (left <= x) && (right >= x)

applyRules :: [Rule] -> Ticket -> [Integer]
applyRules rules = filter isInvalid
  where
    isInvalid x = all (isOutOfRule x) $ map rRanges rules
    isOutOfRule x ranges = all (not . inRange x) ranges

invalidValues :: [Rule] -> [Ticket] -> [Integer]
invalidValues rules tickets = concat $ map (applyRules rules) tickets

parseInput :: Parser a -> IO a
parseInput parser = parseInput' parser >>= either report return
  where
    report err = do
      putStrLn $ "Error: " ++ show err
      exitFailure
    parseInput' p = do
      input <- getContents
      return $ parse p "(stdin)" input

main :: IO ()
main = do
  input <- parseInput inputParser
  putStrLn $ show $ sum $ invalidValues (iRules input) (iNearby input)
