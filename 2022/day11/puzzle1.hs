#! /usr/bin/env nix-shell
#! nix-shell -i runhaskell -p

-- https://adventofcode.com/2022/day/11
-- You take some notes (your puzzle input) on the items each monkey currently has,
-- how worried you are about those items, and how the monkey makes decisions based
-- on your worry level.

-- Figure out which monkeys to chase by counting how many items they inspect over
-- 20 rounds. What is the level of monkey business after 20 rounds of
-- stuff-slinging simian shenanigans?

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
data Monkey = Monkey MonkeyId [Item] ChangeOp TestOp MoveOp
type Monkeys = M.Map MonkeyId Monkey

monkeyParser :: Parser Monkey
monkeyParser = do
  _ <- string "Monkey "
  i <- read <$> many1 digit
  _ <- string ":\n  Starting items: "
  items <- sepEndBy1 (read <$> many1 digit) (try $ string ", ")
  _ <- string "\n  Operation: new = old "
  changeOp <- changeParser
  _ <- string "\n  Test: "
  testOp <- testParser
  _ <- string "\n"
  moveOp <- moveParser
  _ <- optional $ string "\n"
  return $ Monkey i items changeOp testOp moveOp

changeParser :: Parser ChangeOp
changeParser = op (+) '+' <|> op (*) '*' <|> power
  where
    op f c = f <$> try (char c *> char ' ' *> (read <$> many1 digit))
    power = (^ 2) <$ try (string "* old")

testParser :: Parser TestOp
testParser = op <$> num
  where
    op a b = mod b a == 0
    num = read <$> (string "divisible by " *> many1 digit)

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
    simulateItem x = (m (t newLevel), newLevel)
      where
        newLevel = c x `div` 3

makeMonkeys :: [Monkey] -> Monkeys
makeMonkeys = M.fromList . map makeMonkeys'
  where
    makeMonkeys' m = (monkeyId m, m)

monkeyId :: Monkey -> MonkeyId
monkeyId (Monkey i _ _ _ _) = i

updateMonkey :: Monkeys -> Monkey -> Monkeys
updateMonkey ms m = M.insert (monkeyId m) m ms

addItem :: Monkey -> Item -> Monkey
addItem (Monkey i items c t m) item = Monkey i (item : items) c t m

addItemTo :: Monkeys -> (MonkeyId, Item) -> Monkeys
addItemTo ms (i, item) = updateMonkey ms (addItem (fromJust $ M.lookup i ms) item)

traceMonkeys :: Monkeys -> [(MonkeyId, Int)]
traceMonkeys ms = traceMonkeys' mids ms
  where
    mids = cycle $ M.keys ms
    traceMonkeys' [] _ = []
    traceMonkeys' (i:is) ms = (i, length $ snd simulated) : traceMonkeys' is next
      where
        simulated = simulateMonkey $ fromJust $ M.lookup i ms
        next = foldl' addItemTo (updateMonkey ms $ fst simulated) (snd simulated)

topActivity :: [(MonkeyId, Int)] -> [(MonkeyId, Int)]
topActivity = sortOn (negate . snd) . map makeState . groupBy sameMonkey . sort
  where
    sameMonkey m1 m2 = fst m1 == fst m2
    makeState l = (fst $ head l, sum $ map snd l)

business :: Int -> [Monkey] -> Int
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
  print $ business 20 monkeys
