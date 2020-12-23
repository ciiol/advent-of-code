#! /usr/bin/env nix-shell
#! nix-shell -i runhaskell -p
{-# LANGUAGE BangPatterns #-}

-- https://adventofcode.com/2020/day/23#part2
-- Your labeling is still correct for the first few cups; after that,
-- the remaining cups are just numbered in an increasing fashion starting
-- from the number after the highest number in your list and proceeding
-- one by one until one million is reached. In this way, every number
-- from one through one million is used exactly once.

-- After discovering where you made the mistake in translating Crab Numbers,
-- you realize the small crab isn't going to do merely 100 moves; the crab
-- is going to do ten million (10000000) moves!

-- Determine which two cups will end up immediately clockwise of cup 1.
-- What do you get if you multiply their labels together?

import qualified Data.List as L
import qualified Data.Map.Strict as M
import qualified Data.Maybe as Mb
import Text.ParserCombinators.Parsec (Parser, many1, parse, digit)
import System.Exit (exitFailure)

type Cup = Integer
data Cups = Cups {cPos :: Cup, cIndex :: M.Map Cup Cup} deriving (Show, Eq, Ord)

singleton :: a -> [a]
singleton x = [x]

cupParser :: Parser Integer
cupParser = (read . singleton) <$> digit

expand :: Integer -> [Cup] -> [Cup]
expand n c = c ++ [((maximum c) + 1) .. n]

makeCups :: [Cup] -> Cups
makeCups cups = Cups (head cups) (M.fromList $ zip cups $ drop 1 $ cycle cups)

getNextCup :: Cup -> Cups -> Cup
getNextCup x = Mb.fromJust . M.lookup x . cIndex

walkFrom :: Cups -> Cup -> [Cup]
walkFrom cups start = next : (walkFrom cups next)
  where
    next = getNextCup start cups

insert :: Cup -> Cup -> Cups -> Cups
insert k v cups = cups {cIndex = (M.insert k v $ cIndex cups)}

next_positions :: Cups -> [Cup]
next_positions cups = walkFrom cups (cPos cups)

shift :: Cups -> Cups
shift cups = cups {cPos = (getNextCup (cPos cups) cups)}

insertAfter :: Cup -> [Cup] -> Cups -> Cups
insertAfter x cups allCups | x == 0 = insertAfter 1000000 cups allCups
insertAfter x cups allCups | L.elem x cups = insertAfter (x - 1) cups allCups
insertAfter x cups allCups = insert x (head cups) $ insert (last cups) (getNextCup x allCups) allCups

playMove :: Cups -> Cups
playMove cups = shift $ insertAfter (current - 1) picked rest
  where
    picked = take 3 $ next_positions cups
    rest = insert current (head $ drop 3 $ next_positions cups) cups
    current = cPos cups

play :: Cups -> [Cups]
play cups = next : play next
  where
    !next = playMove cups

takeAfter :: Int -> Cup -> Cups -> [Cup]
takeAfter n x = take n . flip walkFrom x

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
  cups <- parseInput $ many1 cupParser
  putStrLn $ show $ takeAfter 2 1 $ last $ take 10000000 $ play $ makeCups $ expand 1000000 cups
