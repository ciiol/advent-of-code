#! /usr/bin/env nix-shell
#! nix-shell -i runhaskell -p

-- https://adventofcode.com/2022/day/10
-- The CPU has a single register, X, which starts with the value 1.
-- It supports only two instructions:

--     addx V takes two cycles to complete. After two cycles, the X register
--       is increased by the value V. (V can be negative.)
--     noop takes one cycle to complete. It has no other effect.

-- Find the signal strength during the 20th, 60th, 100th, 140th, 180th,
-- and 220th cycles. What is the sum of these six signal strengths?

import Text.ParserCombinators.Parsec (Parser, parse, (<|>), try, sepEndBy1, many1, string, digit, char)
import System.Exit (exitFailure)

data Command = AddX Int | Noop deriving (Eq, Show)

commandParser :: Parser Command
commandParser = 
  (AddX <$> (try (string "addx") *> char ' ' *> (read <$> many1 (digit <|> char '-'))))
  <|> (Noop <$ try (string "noop"))

simulate :: Int -> [Command] -> [Int]
simulate x [] = repeat x
simulate x (Noop : cs) = x : simulate x cs
simulate x ((AddX dx) : cs) = x : x : simulate (x + dx) cs

takeSignalAt :: [Int] -> [Int] -> [Int]
takeSignalAt ns = takeSignalAt' ns . zip [1..]
  where
    takeSignalAt' (n:ns) ((i, x):xs) | i == n = x * n : takeSignalAt' ns xs
    takeSignalAt' [] _ = []
    takeSignalAt' ns (_:xs) = takeSignalAt' ns xs
    takeSignalAt' _ _ = error "oops"

parseInput :: Parser a -> IO a
parseInput parser = parseInput' parser >>= either report return
  where
    report err = do
      putStrLn $ "Error: " ++ show err
      exitFailure
    parseInput' p = do parse p "(stdin)" <$> getContents

main :: IO ()
main = do
  commands <- parseInput $ sepEndBy1 commandParser $ char '\n'
  print $ sum $ takeSignalAt [20, 60, 100, 140, 180, 220] $ simulate 1 commands
