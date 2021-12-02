#! /usr/bin/env nix-shell
#! nix-shell -i runhaskell -p

-- https://adventofcode.com/2021/day/2
-- In addition to horizontal position and depth, you'll also need to track a third value,
-- aim, which also starts at 0. The commands also mean something entirely different than you first thought:

--     down X increases your aim by X units.
--     up X decreases your aim by X units.
--     forward X does two things:
--         It increases your horizontal position by X units.
--         It increases your depth by your aim multiplied by X.

-- What do you get if you multiply your final horizontal position by your final depth?


import Text.Parsec.Language (emptyDef)
import System.Exit (exitFailure)
import Data.Functor (($>))
import Data.List (foldl')
import Control.Arrow ((&&&))
import qualified Text.ParserCombinators.Parsec.Token as P
import Text.ParserCombinators.Parsec (Parser, many1, endBy1, parse, try, string, newline, (<|>))

type Change = Integer
data Command = Forward Change | Down Change | Up Change deriving (Show, Eq, Ord)
type Coord = (Integer, Integer)
type Aim = Integer
data Position = Position Aim Coord deriving (Show, Eq, Ord)

lexer = P.makeTokenParser emptyDef

trySymbol :: String -> Parser String
trySymbol = P.symbol lexer

commandsParser :: Parser [Command]
commandsParser = many1 commandParser

commandNameParser :: Parser (Change -> Command)
commandNameParser =
    trySymbol "forward" $> Forward
    <|> trySymbol "down" $> Down
    <|> trySymbol "up" $> Up

commandParser :: Parser Command
commandParser = do
  name <- commandNameParser
  value <- P.natural lexer
  return $ name value

start :: Position
start = Position 0 (0, 0)

move :: Position -> Command -> Position
move (Position aim (x, y)) (Forward dx) = Position aim (x + dx, y + dx * aim)
move (Position aim (x, y)) (Down dy) = Position (aim + dy) (x, y)
move (Position aim (x, y)) (Up dy) = Position (aim - dy) (x, y)

result :: Position -> Integer 
result (Position _aim (x, y)) = x * y

parseInput :: Parser a -> IO a
parseInput parser = parseInput' parser >>= either report return
  where
    report err = do
        putStrLn $ "Error: " ++ show err
        exitFailure
    parseInput' p = do parse p "(stdin)" <$> getContents

main :: IO ()
main = do
    commands <- parseInput commandsParser
    print $ result $ foldl' move start commands
