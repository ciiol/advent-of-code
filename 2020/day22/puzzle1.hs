#! /usr/bin/env nix-shell
#! nix-shell -i runhaskell -p

-- https://adventofcode.com/2020/day/22
-- Before the game starts, split the cards so each player has their own deck (your puzzle input).
-- Then, the game consists of a series of rounds: both players draw their top card, and the
-- player with the higher-valued card wins the round. The winner keeps both cards, placing them
-- on the bottom of their own deck so that the winner's card is above the other card.
-- If thiscauses a player to have all of the cards, they win, and the game ends.

-- Play the small crab in a game of Combat using the two decks you just dealt.
-- What is the winning player's score?

import qualified Text.ParserCombinators.Parsec.Token as P
import qualified Data.List as L
import Text.ParserCombinators.Parsec (Parser, many1, parse, many1)
import Text.Parsec.Language (emptyDef)
import System.Exit (exitFailure)

type Card = Integer
type Hand = [Card]

handParser :: Parser Hand
handParser = do
  _ <- P.symbol lexer "Player "
  _ <- P.natural lexer
  _ <- P.symbol lexer ":"
  cards <- many1 $ P.natural lexer
  return cards

lexer = P.makeTokenParser emptyDef

playRound :: [Hand] -> [Hand]
playRound = addToWiner . reverse . L.sort
  where
    addToWiner [] = []
    addToWiner hands@(h:hs) = ((tail h) ++ (concat $ map (take 1) hands)) : (map (drop 1) hs)

play :: [Hand] -> [[Hand]]
play hands | (length $ filter (/= []) hands) > 1 = hands : (play $ playRound hands)
play hands = [hands]

score :: Hand -> Integer
score = sum . map score' . zip [1..] . reverse
  where
    score' (i, n) = i * n

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
  hands <- parseInput $ many1 handParser
  putStrLn $ show $ score $ head $ last $ play hands
