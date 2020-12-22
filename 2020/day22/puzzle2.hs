#! /usr/bin/env nix-shell
#! nix-shell -i runhaskell -p

-- https://adventofcode.com/2020/day/22#part2
-- Recursive Combat still starts by splitting the cards into two decks
-- (you offer to play with the same starting decks as before - it's only fair).
-- Then, the game consists of a series of rounds with a few changes:

--     Before either player deals a card, if there was a previous round in
--     this game that had exactly the same cards in the same order in the same
--     players' decks, the game instantly ends in a win for player 1. Previous
--     rounds from other games are not considered. (This prevents infinite
--     games of Recursive Combat, which everyone agrees is a bad idea.)
--     Otherwise, this round's cards must be in a new configuration; the
--     players begin the round by each drawing the top card of their deck as normal.
--     If both players have at least as many cards remaining in their deck as the
--     value of the card they just drew, the winner of the round is determined by
--     playing a new game of Recursive Combat (see below).
--     Otherwise, at least one player must not have enough cards left in their deck
--     to recurse; the winner of the round is the player with the higher-value card.

-- Defend your honor as Raft Captain by playing the small crab in a game of
--   Recursive Combat using the same two decks as before. What is the winning player's score?

import qualified Text.ParserCombinators.Parsec.Token as P
import qualified Data.List as L
import qualified Data.Set as S
import Text.ParserCombinators.Parsec (Parser, many1, parse, many1)
import Text.Parsec.Language (emptyDef)
import System.Exit (exitFailure)

type Card = Integer
data Hand = Hand {hID :: Integer, hCards :: [Card]} deriving (Show, Eq, Ord)

handParser :: Parser Hand
handParser = do
  _ <- P.symbol lexer "Player "
  hadnID <- P.natural lexer
  _ <- P.symbol lexer ":"
  cards <- many1 $ P.natural lexer
  return $ Hand hadnID cards

lexer = P.makeTokenParser emptyDef

updateCards :: ([Card] -> [Card]) -> Hand -> Hand
updateCards f hand = hand {hCards = (f $ hCards hand)}

getCard :: Hand -> Card
getCard = head . hCards

addCards :: Integer -> [Card] -> [Hand] -> [Hand]
addCards i cards hands = map updateHand hands
  where
    updateHand h | (hID h) == i = updateCards (flip (++) cards') h
    updateHand h = h
    winnerCard = fst . head . filter ((== i) . hID . snd) $ zip cards hands
    cards' = winnerCard : (filter (/= winnerCard) cards)

takeTopCards :: [Hand] -> [(Card, Hand)]
takeTopCards hands = zip (map getCard hands) (map (updateCards $ drop 1) hands)

canRecursive :: [(Card, Hand)] -> Bool
canRecursive = all (\ (c, h) -> c <= (fromIntegral $ length $ hCards h))

makeRecursiveHands :: [(Card, Hand)] -> [Hand]
makeRecursiveHands = map makeHand
  where
    makeHand (c, h) = updateCards (take $ fromIntegral c) h

getWinner :: [(Card, Hand)] -> Integer
getWinner hands | canRecursive hands = hID $ playGame $ makeRecursiveHands hands
getWinner hands = hID $ snd $ head $ L.sortOn (negate . fst) hands

isHandEmpty :: Hand -> Bool
isHandEmpty = (== []) . hCards

playRound :: [Hand] -> [Hand]
playRound = addToWinner . takeTopCards
  where
    addToWinner hs = addCards (getWinner hs) (cards hs) (hands hs)
    cards = map fst
    hands = map snd

play :: [Hand] -> [[Hand]]
play hands | (length $ filter (not . isHandEmpty) hands) > 1 = hands : (play $ playRound hands)
play hands = [hands]

playGame :: [Hand] -> Hand
playGame = playGame' S.empty . play
  where
    playGame' _hist [] = error "oops"
    playGame' _hist [x] = head $ filter (not . isHandEmpty) x
    playGame' hist (x:_) | S.member x hist = head $ filter ((== 1) . hID) x
    playGame' hist (x:xs) = playGame' (S.insert x hist) xs

score :: Hand -> Integer
score = sum . map score' . zip [1..] . reverse . hCards
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
  putStrLn $ show $ score $ playGame hands
