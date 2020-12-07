#! /usr/bin/env nix-shell
#! nix-shell -i runhaskell -p

-- https://adventofcode.com/2020/day/7#part2
-- For example, consider the following rules:

-- light red bags contain 1 bright white bag, 2 muted yellow bags.
-- dark orange bags contain 3 bright white bags, 4 muted yellow bags.
-- bright white bags contain 1 shiny gold bag.
-- muted yellow bags contain 2 shiny gold bags, 9 faded blue bags.
-- shiny gold bags contain 1 dark olive bag, 2 vibrant plum bags.
-- dark olive bags contain 3 faded blue bags, 4 dotted black bags.
-- vibrant plum bags contain 5 faded blue bags, 6 dotted black bags.
-- faded blue bags contain no other bags.
-- dotted black bags contain no other bags.

-- How many individual bags are required inside your single shiny gold bag?

import qualified Data.Map as M
import qualified Data.Set as S
import qualified Text.ParserCombinators.Parsec.Token as P
import Text.ParserCombinators.Parsec (Parser, (<|>), try, many, parse)
import Text.Parsec.Language (emptyDef)
import System.Exit (exitFailure)

data Bag = Bag String String deriving (Show, Eq, Ord)
data InnerBag = InnerBag Integer Bag deriving (Show, Eq, Ord)
data Rule = Rule Bag [InnerBag] deriving (Show, Eq, Ord)

rulesParser :: Parser [Rule]
rulesParser = many ruleParser

ruleParser :: Parser Rule
ruleParser = do
  outer <- bagParser
  _ <- P.symbol lexer "contain"
  inner <- innerBagsList
  _ <- P.dot lexer
  return $ Rule outer inner

innerBagParser :: Parser InnerBag
innerBagParser = do
  number <- P.natural lexer
  bagName <- bagParser
  return $ InnerBag number bagName

innerBagsList :: Parser [InnerBag]
innerBagsList = (P.commaSep1 lexer innerBagParser) <|> noInnerBagParser

noInnerBagParser :: Parser [InnerBag]
noInnerBagParser = do
  _ <- (try (P.symbol lexer "no other bags"))
  return []

bagParser :: Parser Bag
bagParser = do
  first <- colourPartParser
  second <- colourPartParser
  _ <- (try (P.symbol lexer "bags") <|> (try (P.symbol lexer "bag")))
  return $ Bag first second

lexer = P.makeTokenParser emptyDef

colourPartParser :: Parser String
colourPartParser = P.identifier lexer

getInners :: Rule -> [InnerBag]
getInners (Rule _outer inners) = inners

getRule :: [Rule] -> Bag -> Rule
getRule ((Rule outer inners):rules) start | outer == start = (Rule outer inners)
getRule (_:rules) start = getRule rules start

bagsInside :: [Rule] -> Bag -> Integer
bagsInside rules start =
  sum $ map bagsInside' inners
  where
    inners = getInners $ getRule rules start
    bagsInside' (InnerBag num bag) = num * (bagsInside rules bag + 1)

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
  rules <- parseInput rulesParser
  putStrLn $ show $ bagsInside rules (Bag "shiny" "gold")
