#! /usr/bin/env nix-shell
#! nix-shell -i runhaskell -p

-- https://adventofcode.com/2022/day/16
-- You scan the cave for other options and discover a network of pipes and pressure-release valves.
-- You aren't sure how such a system got into a volcano, but you don't have time to complain;
-- your device produces a report (your puzzle input) of each valve's flow rate if it were opened
-- (in pressure per minute) and the tunnels you could use to move between the valves.

-- There's even a valve in the room you and the elephants are currently standing in labeled AA.
-- You estimate it will take you one minute to open a single valve and one minute to follow any
-- tunnel from one valve to another. 

-- Work out the steps to release the most pressure in 30 minutes. What is the most pressure you can release?

import Text.ParserCombinators.Parsec (Parser, parse, try, many1, sepEndBy1, char, string, digit, letter, optional)
import System.Exit (exitFailure)
import Data.Maybe (fromJust)
import Data.List (foldl')
import qualified Data.Set as S
import qualified Data.Map.Strict as M

type ValveId = String
data Valve = Valve ValveId Int [ValveId] deriving (Eq, Show)
type Valves = M.Map ValveId Valve

valveParser :: Parser Valve
valveParser = do 
  _ <- string "Valve "
  i <- many1 letter
  _ <- string " has flow rate="
  rate <- read <$> many1 digit
  _ <- string "; tunnel" *> optional (char 's')
  _ <- string " lead"  *> optional (char 's')
  _ <- string " to valve" *> optional (char 's') *> char ' '
  l <- sepEndBy1 (many1 letter) (try $ string ", ")
  return $ Valve i rate l

makeValves :: [Valve] -> Valves
makeValves = M.fromList . map makeValves'
  where
    makeValves' v = (valveId v, v)

flow :: Valve -> Int
flow (Valve _ f _) = f

linked :: Valve -> [ValveId]
linked (Valve _ _ l) = l

valveId :: Valve -> ValveId
valveId (Valve i _ _) = i

closedValves :: Valves -> S.Set ValveId
closedValves vs = S.fromList $ filter canBeOpened $ M.keys vs
  where
    valve i = fromJust $ M.lookup i vs
    canBeOpened = (0 <) . flow . valve

type Cache = M.Map (ValveId, Int, S.Set ValveId) Int

findMaxFlow :: Valves -> ValveId -> Int -> Int
findMaxFlow vs v t = fromJust $ M.lookup (v, t, closed) cache
  where
    closed = closedValves vs
    cache = buildCache vs closed t M.empty v

buildCache :: Valves -> S.Set ValveId -> Int -> Cache -> ValveId -> Cache
buildCache vs closed t c start | (start, t, closed) `M.member` c = c
buildCache vs closed t c start | closed == S.empty || t <= 0 = M.insert (start, t, closed) 0 c
buildCache vs closed t c start = M.insert (start, t, closed) predicted cache'
  where
    isOpened = not $ S.member start closed
    valve' = fromJust $ M.lookup start vs
    linked' = linked valve'
    flow' = if isOpened then 0 else flow valve' * (t - 1)
    closed' = S.delete start closed
    nextSteps = nextStepsIfOpen ++ nextStepsIfNotOpen
    nextStepsIfOpen = if isOpened then [] else [(v, t - 2, closed') | v <- linked']
    nextStepsIfNotOpen = [(v, t - 1, closed) | v <- linked']
    cache' = foldl' updateCache c nextSteps
    updateCache acc (v, t, c) = buildCache vs c t acc v
    predictedIfOpen = if isOpened then 0 else maximum $ map ((+ flow') . fromJust . flip M.lookup cache') nextStepsIfOpen
    predictedIfNotOpen = maximum $ map (fromJust . flip M.lookup cache') nextStepsIfNotOpen
    predicted = max predictedIfOpen predictedIfNotOpen

parseInput :: Parser a -> IO a
parseInput parser = parseInput' parser >>= either report return
  where
    report err = do
      putStrLn $ "Error: " ++ show err
      exitFailure
    parseInput' p = do parse p "(stdin)" <$> getContents

main :: IO ()
main = do
  valves <- parseInput $ makeValves <$> sepEndBy1 valveParser (char '\n')
  print $ findMaxFlow valves "AA" 30 
