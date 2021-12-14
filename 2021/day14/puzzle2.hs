#! /usr/bin/env nix-shell
#! nix-shell -i runhaskell -p

-- https://adventofcode.com/2021/day/14#part2
-- The submarine manual contains instructions for finding the optimal polymer formula;
-- specifically, it offers a polymer template and a list of pair insertion rules
-- (your puzzle input). You just need to work out what polymer would result after
-- repeating the pair insertion process a few times.

-- Apply 40 steps of pair insertion to the polymer template and find the most and least
-- common elements in the result. What do you get if you take the quantity of the most
-- common element and subtract the quantity of the least common element?

import Data.List (concatMap, foldl', groupBy, sort)
import qualified Data.Map.Strict as M
import Data.Maybe (fromJust, fromMaybe)
import System.Exit (exitFailure)
import Text.ParserCombinators.Parsec (Parser, alphaNum, char, many1, parse, skipMany, string)

type Template = M.Map Pair Integer

type Pair = (Char, Char)

type Rules = M.Map Pair Char

inputParser :: Parser (Template, Rules)
inputParser = (,) <$> (makeTemplate <$> many1 alphaNum) <*> (M.fromList <$> many1 ruleParser)

ruleParser :: Parser (Pair, Char)
ruleParser = do
  _ <- skipMany $ char '\n'
  from <- (,) <$> alphaNum <*> alphaNum
  _ <- string " -> "
  to <- alphaNum
  return (from, to)

makeTemplate :: [Char] -> Template
makeTemplate chars = foldl' (addToTemplate 1) M.empty pairs
  where
    terminated = "*" ++ chars ++ "*"
    pairs = zip terminated $ tail terminated

addToTemplate :: Integer -> Template -> Pair -> Template
addToTemplate num t p = M.alter (Just . (num +) . fromMaybe 0) p t

applyRules :: Template -> Rules -> Template
applyRules template rules = foldl' applyRules' M.empty $ M.toList template
  where
    applyRules' t (x@(a, b), num) | x `M.member` rules = addToTemplate num (addToTemplate num t (a, c)) (c, b)
      where
        c = fromJust $ M.lookup x rules
    applyRules' t (x, num) = addToTemplate num t x

result :: Template -> Rules -> Int -> Integer
result template rules num = most - least
  where
    final = foldl applyRules template $ replicate num rules
    toNum ((a, b), num) | a == '*' = []
    toNum ((a, b), num) = [(a, num)]
    allLetters = concatMap toNum $ M.toList final
    letters = map (\x -> (fst $ head x, sum $ map snd x)) $ groupBy (\a b -> fst a == fst b) $ sort allLetters
    least = minimum $ map snd letters
    most = maximum $ map snd letters

parseInput :: Parser a -> IO a
parseInput parser = parseInput' parser >>= either report return
  where
    report err = do
      putStrLn $ "Error: " ++ show err
      exitFailure
    parseInput' p = do parse p "(stdin)" <$> getContents

main :: IO ()
main = do
  (template, rules) <- parseInput inputParser
  print $ result template rules 40
