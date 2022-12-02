#! /usr/bin/env nix-shell
#! nix-shell -i runhaskell -p

-- https://adventofcode.com/2022/day/2
-- The Elves begin to set up camp on the beach. To decide whose tent gets to be closest to
-- the snack storage, a giant Rock Paper Scissors tournament is already in progress.

-- The first column is what your opponent is going to play: A for Rock, B for Paper, and
-- C for Scissors. The second column, you reason, must be what you should play in response:
-- X for Rock, Y for Paper, and Z for Scissors.

-- The score for a single round is the score for the shape you selected
-- (1 for Rock, 2 for Paper, and 3 for Scissors) plus the score for the outcome of
-- the round (0 if you lost, 3 if the round was a draw, and 6 if you won).

-- What would your total score be if everything goes exactly according to your strategy guide?

import Text.ParserCombinators.Parsec (Parser, sepEndBy1, skipMany1, char, parse, (<|>))
import System.Exit (exitFailure)
import Data.Functor (($>))

data Action = A | B | C deriving (Show, Eq)
data Response = X | Y | Z deriving (Show, Eq)
data Result = Won | Lost | Draw deriving (Show, Eq)

type Rule = (Action, Response)

actionParser :: Parser Action
actionParser =
  char 'A' $> A
  <|> char 'B' $> B
  <|> char 'C' $> C

responseParser :: Parser Response
responseParser =
  char 'X' $> X
  <|> char 'Y' $> Y
  <|> char 'Z' $> Z

ruleParser :: Parser Rule
ruleParser = (,) <$> actionParser <*> (skipMany1 (char ' ') *> responseParser)

guideParser :: Parser [Rule]
guideParser = sepEndBy1 ruleParser (char '\n')

scoreGuide :: [Rule] -> Int
scoreGuide = sum . map score

score :: Rule -> Int
score (a, b) = scoreRespose b + scoreResult (rule a b)
  where
    scoreResult Won = 6
    scoreResult Lost = 0
    scoreResult Draw = 3
    scoreRespose X = 1
    scoreRespose Y = 2
    scoreRespose Z = 3

rule :: Action -> Response -> Result
rule A X = Draw
rule A Y = Won
rule A Z = Lost
rule B X = Lost
rule B Y = Draw
rule B Z = Won
rule C X = Won
rule C Y = Lost
rule C Z = Draw

parseInput :: Parser a -> IO a
parseInput parser = parseInput' parser >>= either report return
  where
    report err = do
      putStrLn $ "Error: " ++ show err
      exitFailure
    parseInput' p = do parse p "(stdin)" <$> getContents

main :: IO ()
main = do
  guide <- parseInput guideParser
  print $ scoreGuide guide
