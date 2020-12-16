#! /usr/bin/env nix-shell
#! nix-shell -i runhaskell -p

-- https://adventofcode.com/2020/day/16#part2
-- Now that you've identified which tickets contain invalid values,
-- discard those tickets entirely. Use the remaining valid tickets
-- to determine which field is which.

-- Once you work out which field is which, look for the six fields on
-- your ticket that start with the word departure.
-- What do you get if you multiply those six values together?

import qualified Text.ParserCombinators.Parsec.Token as P
import qualified Data.Set as S
import Data.List (find, isPrefixOf)
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

inRule :: Integer -> Rule -> Bool
inRule x = any (inRange x) . rRanges

applicableRules :: [Integer] -> [Rule] -> [Rule]
applicableRules nums = filter applicableRule
  where
    applicableRule rule = all (flip inRule rule) nums

isTicketValid :: [Rule] -> Ticket -> Bool
isTicketValid rules = all isValid
  where
    isValid x = any (inRule x) rules

onlyValid :: [Rule] -> [Ticket] -> [Ticket]
onlyValid rules = filter (isTicketValid rules)

lzip :: [a] -> [[a]] -> [(a, [a])]
lzip (t:ts) lists = (t, map head lists) : lzip ts (map tail lists)
lzip [] _ = []

possibleDecodings :: [Rule] -> Ticket -> [Ticket] -> [(Integer, S.Set String)]
possibleDecodings rules your nearby = map possibleDecodings' $ lzip your nearby
  where
    possibleDecodings' (y, ns) = (y, S.fromList $ map rName $ applicableRules ns rules)

deduce :: [(Integer, S.Set String)] -> [(Integer, String)]
deduce [] = []
deduce names = (cleared obvious) : (deduce $ map (intersectNames obvious) $ filter (/= obvious) names)
  where
    obvious = maybe (error "oops") id $ find ((== 1) . S.size . snd) names
    cleared (n, s) = (n, head $ S.toList s)
    intersectNames (_, onames) (n, ns) = (n, S.difference ns onames)

makeResult :: [(Integer, String)] -> Integer
makeResult decodings = foldl (*) 1 $ map fst $ departure
  where
    departure = filter ((isPrefixOf "departure") . snd) decodings

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
  putStrLn $ show $ makeResult $ deduce $ possibleDecodings (iRules input) (iYour input) (onlyValid (iRules input) $ iNearby input)
