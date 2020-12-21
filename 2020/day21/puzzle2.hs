#! /usr/bin/env nix-shell
#! nix-shell -i runhaskell -p

-- https://adventofcode.com/2020/day/21#part2
-- Now that you've isolated the inert ingredients, you should have enough
-- information to figure out which ingredient contains which allergen.

-- Arrange the ingredients alphabetically by their allergen and separate them
-- by commas to produce your canonical dangerous ingredient list.
-- (There should not be any spaces in your canonical dangerous ingredient
-- list.)

-- Time to stock your raft with supplies.
-- What is your canonical dangerous ingredient list?

import qualified Data.Set as S
import qualified Data.List as L
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

deduceAllergens :: Foods -> Mapping
deduceAllergens = deduceAllergens' . sorted . allergensMapping
  where
    sorted = L.sortOn (S.size . snd)
    deduceAllergens' [] = []
    deduceAllergens' (x:xs) = x : (deduceAllergens' $ sorted $ map (exclude x) xs)
    exclude (_, s1) (v, s2) = (v, S.difference s2 s1)

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
  putStrLn $ show $ L.intercalate "," $ map (concat . S.toList . snd) $ L.sortOn fst $ deduceAllergens foods
