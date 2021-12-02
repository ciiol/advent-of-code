#! /usr/bin/env nix-shell
#! nix-shell -i runhaskell -p

-- https://adventofcode.com/2021/day/2
-- Now, you need to figure out how to pilot this thing.

-- It seems like the submarine can take a series of commands like forward 1, down 2, or up 3:

--     forward X increases the horizontal position by X units.
--     down X increases the depth by X units.
--     up X decreases the depth by X units.

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

move :: Coord -> Command -> Coord
move (x, y) (Forward dx) = (x + dx, y)
move (x, y) (Down dy) = (x, y + dy)
move (x, y) (Up dy) = (x, y - dy)

result :: Coord -> Integer 
result (x, y) = x * y

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
    print $ result $ foldl' move (0, 0) commands
