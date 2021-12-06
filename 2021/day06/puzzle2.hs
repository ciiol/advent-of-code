#! /usr/bin/env nix-shell
#! nix-shell -i runhaskell -p

-- https://adventofcode.com/2021/day/6
-- You know nothing about this specific species of lanternfish,
-- you make some guesses about their attributes. Surely, each
-- lanternfish creates a new lanternfish once every 7 days.

-- However, this process isn't necessarily synchronized between
-- every lanternfish - one lanternfish might have 2 days left until
-- it creates another lanternfish, while another might have 4. So,
-- you can model each fish as a single number that represents the
-- number of days until it creates a new lanternfish.

-- Furthermore, you reason, a new lanternfish would surely need
-- slightly longer before it's capable of producing more lanternfish:
-- two more days for its first cycle.

-- How many lanternfish would there be after 256 days?

import Data.List (concatMap)
import System.Exit (exitFailure)
import Text.Parsec.Language (emptyDef)
import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe)
import Text.ParserCombinators.Parsec (Parser, char, parse, sepBy1)
import qualified Text.ParserCombinators.Parsec.Token as P

type Fish = Integer
type Model = M.Map Fish Integer 

lexer = P.makeTokenParser emptyDef

fishesParser :: Parser [Fish]
fishesParser = sepBy1 (P.natural lexer) (char ',')

makeModel :: [Fish] -> Model
makeModel = foldl (addToModel 1) M.empty

addToModel :: Integer -> Model -> Fish -> Model
addToModel x model f = M.alter (Just . maybe x (+ x)) f model

fishesAt :: Model -> Fish -> Integer
fishesAt model f = fromMaybe 0 $ M.lookup f model

simulate :: Model -> [Model]
simulate model = model : simulate next
  where
    next = addToModel zeroAge evolved 8
    zeroAge = fishesAt model 0
    reducedAge = M.fromList [(f - 1, x) | (f, x) <- M.toList model, f > 0]
    evolved = addToModel zeroAge reducedAge 6

total :: Model -> Integer
total = M.foldl' (+) 0

parseInput :: Parser a -> IO a
parseInput parser = parseInput' parser >>= either report return
  where
    report err = do
      putStrLn $ "Error: " ++ show err
      exitFailure
    parseInput' p = do parse p "(stdin)" <$> getContents

main :: IO ()
main = do
  fishes <- parseInput fishesParser
  print $ total $ simulate (makeModel fishes) !! 256
