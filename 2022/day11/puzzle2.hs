#! /usr/bin/env nix-shell
#! nix-shell -i runhaskell -p

-- https://adventofcode.com/2022/day/11
-- You take some notes (your puzzle input) on the items each monkey currently has,
-- how worried you are about those items, and how the monkey makes decisions based
-- on your worry level.

-- You're worried you might not ever get your items back. So worried, in fact, that your relief that a monkey's
-- inspection didn't damage an item no longer causes your worry level to be divided by three.

-- Worry levels are no longer divided by three after each item is inspected; you'll need to find
-- another way to keep your worry levels manageable. Starting again from the initial state in your
-- puzzle input, what is the level of monkey business after 10000 rounds?

import Text.ParserCombinators.Parsec (Parser, parse, try, sepEndBy1, many1, (<|>), digit, char, string, optional)
import System.Exit (exitFailure)
import Data.Maybe (fromJust)
import Data.List (foldl', sort, groupBy, sortOn)
import qualified Data.Map.Strict as M

type Item = Integer
type MonkeyId = Int
type ChangeOp = (Item -> Item)
type TestOp = (Item -> Bool)
type MoveOp = (Bool -> MonkeyId)
data Monkey = Monkey MonkeyId [Item] ChangeOp Integer MoveOp
type Monkeys = M.Map MonkeyId Monkey

monkeyParser :: Parser Monkey
monkeyParser = do
  _ <- string "Monkey "
  i <- read <$> many1 digit
  _ <- string ":\n  Starting items: "
  items <- sepEndBy1 (read <$> many1 digit) (try $ string ", ")
  _ <- string "\n  Operation: new = old "
  changeOp <- changeParser
  _ <- string "\n  Test: divisible by "
  divisor <- read <$> many1 digit
  _ <- string "\n"
  moveOp <- moveParser
  _ <- optional $ string "\n"
  return $ Monkey i items changeOp divisor moveOp

changeParser :: Parser ChangeOp
changeParser = op (+) '+' <|> op (*) '*' <|> power
  where
    op f c = f <$> try (char c *> char ' ' *> (read <$> many1 digit))
    power = (^ 2) <$ try (string "* old")

moveParser :: Parser MoveOp
moveParser = op <$> mont <*> (char '\n' *> monf)
  where
    mont = read <$> (string "    If true: throw to monkey " *> many1 digit)
    monf = read <$> (string "    If false: throw to monkey " *> many1 digit)
    op a b t | t = a
    op a b _ = b

simulateMonkey :: Monkey -> (Monkey, [(MonkeyId, Item)])
simulateMonkey (Monkey i items c t m) = (newMonkey, map simulateItem items)
  where
    newMonkey = Monkey i [] c t m
    simulateItem x = (m (mod newLevel t == 0), newLevel)
      where
        newLevel = c x

makeMonkeys :: [Monkey] -> Monkeys
makeMonkeys = M.fromList . map makeMonkeys'
  where
    makeMonkeys' m = (monkeyId m, m)

monkeyId :: Monkey -> MonkeyId
monkeyId (Monkey i _ _ _ _) = i

monkeyDivisor :: Monkey -> Integer
monkeyDivisor (Monkey _ _ _ t _) = t

updateMonkey :: Monkeys -> Monkey -> Monkeys
updateMonkey ms m = M.insert (monkeyId m) m ms

addItem :: Monkey -> Item -> Monkey
addItem (Monkey i items c t m) item = Monkey i (item : items) c t m

addItemTo :: Monkeys -> (MonkeyId, Item) -> Monkeys
addItemTo ms (i, item) = updateMonkey ms (addItem (fromJust $ M.lookup i ms) item)

reduceWorryLevel :: Integer -> [(MonkeyId, Item)] -> [(MonkeyId, Item)]
reduceWorryLevel d = map reduceWorryLevel'
  where
    reduceWorryLevel' (i, x) = (i, x `mod` d)

traceMonkeys :: Monkeys -> [(MonkeyId, Integer)]
traceMonkeys ms = traceMonkeys' mids ms
  where
    mids = cycle $ M.keys ms
    commonDivisor = product $ map monkeyDivisor $ M.elems ms
    traceMonkeys' [] _ = []
    traceMonkeys' (i:is) ms = (i, fromIntegral $ length $ snd simulated) : traceMonkeys' is next
      where
        simulated = simulateMonkey $ fromJust $ M.lookup i ms
        reduced = reduceWorryLevel commonDivisor $ snd simulated
        next = foldl' addItemTo (updateMonkey ms $ fst simulated) reduced

topActivity :: [(MonkeyId, Integer)] -> [(MonkeyId, Integer)]
topActivity = sortOn (negate . snd) . map makeState . groupBy sameMonkey . sort
  where
    sameMonkey m1 m2 = fst m1 == fst m2
    makeState l = (fst $ head l, sum $ map snd l)

business :: Int -> [Monkey] -> Integer
business n ms = product $ map snd $ take 2 top
  where
    top = topActivity $ take (n * length ms) $ traceMonkeys $ makeMonkeys ms

parseInput :: Parser a -> IO a
parseInput parser = parseInput' parser >>= either report return
  where
    report err = do
      putStrLn $ "Error: " ++ show err
      exitFailure
    parseInput' p = do parse p "(stdin)" <$> getContents

main :: IO ()
main = do
  monkeys <- parseInput $ sepEndBy1 monkeyParser (char '\n')
  print $ business 10000 monkeys
