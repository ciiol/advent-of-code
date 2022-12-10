#! /usr/bin/env nix-shell
#! nix-shell -i runhaskell -p

-- https://adventofcode.com/2022/day/10#part2
-- The CPU has a single register, X, which starts with the value 1.
-- It supports only two instructions:

--     addx V takes two cycles to complete. After two cycles, the X register
--       is increased by the value V. (V can be negative.)
--     noop takes one cycle to complete. It has no other effect.

-- Find the signal strength during the 20th, 60th, 100th, 140th, 180th,
-- and 220th cycles. What is the sum of these six signal strengths?

import Text.ParserCombinators.Parsec (Parser, parse, (<|>), try, sepEndBy1, many1, string, digit, char)
import System.Exit (exitFailure)
import qualified Data.Set as S

data Command = AddX Int | Noop deriving (Eq, Show)

commandParser :: Parser Command
commandParser = 
  (AddX <$> (try (string "addx") *> char ' ' *> (read <$> many1 (digit <|> char '-'))))
  <|> (Noop <$ try (string "noop"))

chunk :: Int -> [a] -> [[a]]
chunk _ [] = []
chunk n xs = take n xs : chunk n (drop n xs)

simulate :: Int -> [Command] -> [Int]
simulate x [] = repeat x
simulate x (Noop : cs) = x : simulate x cs
simulate x ((AddX dx) : cs) = x : x : simulate (x + dx) cs

render :: [Int] -> String
render = zipWith renderPixel (cycle [0..39])
  where
    renderPixel x v | abs (x - v) <= 1 = '#'
    renderPixel _ _ = ' '

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
  mapM_ print $ take 6 $ chunk 40 $ render $ simulate 1 commands
