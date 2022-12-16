#! /usr/bin/env nix-shell
#! nix-shell -i runhaskell -p

-- https://adventofcode.com/2022/day/16#part2
-- You scan the cave for other options and discover a network of pipes and pressure-release valves.
-- You aren't sure how such a system got into a volcano, but you don't have time to complain;
-- your device produces a report (your puzzle input) of each valve's flow rate if it were opened
-- (in pressure per minute) and the tunnels you could use to move between the valves.

-- There's even a valve in the room you and the elephants are currently standing in labeled AA.
-- You estimate it will take you one minute to open a single valve and one minute to follow any
-- tunnel from one valve to another. 

-- You're worried that even with an optimal approach, the pressure released won't be enough.
-- What if you got one of the elephants to help you?

-- With you and an elephant working together for 26 minutes, what is the most pressure you could release?

import Text.ParserCombinators.Parsec (Parser, parse, try, many1, sepEndBy1, char, string, digit, letter, optional)
import System.Exit (exitFailure)
import Data.Maybe (fromJust, fromMaybe, maybe)
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

type Cache = M.Map (ValveId, Int, S.Set ValveId) (Int, S.Set ValveId)

findMaxFlow :: Valves -> ValveId -> Int -> Int
findMaxFlow vs v t = fst firstAttempt + fst secondAttempt
  where
    closed0 = closedValves vs
    cache0 = buildCache vs closed0 t M.empty v
    firstAttempt = fromJust $ M.lookup (v, t, closed0) cache0
    opened = snd firstAttempt
    closed1 = S.difference closed0 opened
    cache1 = buildCache vs closed1 t cache0 v
    secondAttempt = fromJust $ M.lookup (v, t, closed1) cache1

buildCache :: Valves -> S.Set ValveId -> Int -> Cache -> ValveId -> Cache
buildCache vs closed t c pos | (pos, t, closed) `M.member` c = c
buildCache vs closed t c pos | closed == S.empty || t <= 0 = M.insert (pos, t, closed) (0, S.empty) c
buildCache vs closed t c pos = M.insert (pos, t, closed) predictedMax cache'
  where
    isOpened = not $ S.member pos closed
    valve = fromJust $ M.lookup pos vs
    flow' = flow valve
    linked' = linked valve
    open v = S.delete v closed
    nextSteps = nextStepsIfOpen ++ nextStepsIfNotOpen
    nextStepsIfOpen = if isOpened || flow' == 0 then [] else [(pos', t - 2, open pos) | pos' <- linked']
    nextStepsIfNotOpen = [(pos', t - 1, closed) | pos' <- linked']
    cache' = foldl' updateCache c nextSteps
    updateCache acc (v, t, c) = buildCache vs c t acc v
    predicted dx toOpen [] = (0, S.empty)
    predicted dx toOpen steps = maximum $ map (addDx (dx * (t - 1)) toOpen . fromJust . flip M.lookup cache') steps
    addDx dx toOpen (x, opened) = (x + dx, S.union toOpen opened)
    predictedIfOpen = predicted flow' (S.fromList [pos]) nextStepsIfOpen
    predictedIfNotOpen = predicted 0 S.empty nextStepsIfNotOpen
    predictedMax = max predictedIfOpen predictedIfNotOpen

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
  print $ findMaxFlow valves "AA" 26
