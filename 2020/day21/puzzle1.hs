#! /usr/bin/env nix-shell
#! nix-shell -i runhaskell -p

-- https://adventofcode.com/2020/day/21
-- Each allergen is found in exactly one ingredient.
-- Each ingredient contains zero or one allergen
-- Allergens aren't always marked; when they're listed (as in
-- (contains nuts, shellfish) after an ingredients list), the
-- ingredient that contains each listed allergen will be somewhere in
-- the corresponding ingredients list. However, even if an allergen isn't
-- listed, the ingredient that contains that allergen could still be present:
-- maybe they forgot to label it, or maybe it was labeled in a language you don't know.

-- Determine which ingredients cannot possibly contain any of the allergens
-- in your list. How many times do any of those ingredients appear?

import qualified Data.Set as S
import Text.ParserCombinators.Parsec (Parser, many1, parse, many1, endBy1, newline, char, alphaNum, string, try, sepBy1)
import System.Exit (exitFailure)

type Food = ([String], [String])
type Foods = [Food]
type Mapping = [(String, S.Set String)]

foodParser :: Parser ([String], [String])
foodParser = do
  ingredientsList <- endBy1 (many1 alphaNum) (char ' ')
  _ <- string "(contains "
  allergensList <- sepBy1 (many1 alphaNum) (try $ string ", ")
  _ <- string ")"
  return (ingredientsList, allergensList)

uniq :: Ord a => [a] -> [a]
uniq = S.toList . S.fromList

allergens :: Foods -> [String]
allergens = uniq . concat . map snd

ingredients :: Foods -> [String]
ingredients = uniq . concat . map fst

foodsWithAllergen :: String -> Foods -> Foods
foodsWithAllergen i = filter (elem i . snd)

allergensMapping :: Foods -> Mapping
allergensMapping foods = map findPossibleMapping $ allergens foods
  where
    findPossibleMapping i = (i, foldr addToMappting allIngredientsSet $ foodsWithAllergen i foods)
    addToMappting f = S.intersection (S.fromList $ fst f)
    allIngredientsSet = S.fromList $ ingredients foods

withAllergens :: Foods -> S.Set String
withAllergens = foldr S.union S.empty . map snd . allergensMapping

withoutAllergens :: Foods -> S.Set String
withoutAllergens foods = S.difference allIngredientsSet $ withAllergens foods
  where
    allIngredientsSet = S.fromList $ ingredients foods

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
  foods <- parseInput $ endBy1 foodParser newline
  putStrLn $ show $ length $ filter (flip S.member $ withoutAllergens foods) $ concat $ map fst foods
