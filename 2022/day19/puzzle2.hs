#! /usr/bin/env nix-shell
#! nix-shell -i runhaskell -p

-- https://adventofcode.com/2022/day/19
-- To collect the obsidian from the bottom of the pond, you'll need waterproof obsidian-collecting robots.
-- Fortunately, there is an abundant amount of clay nearby that you can use to make them waterproof.

-- In order to harvest the clay, you'll need special-purpose clay-collecting robots. To make any type of
-- robot, you'll need ore, which is also plentiful but in the opposite direction from the clay.

-- Collecting ore requires ore-collecting robots with big drills. Fortunately, you have exactly one
-- ore-collecting robot in your pack that you can use to kickstart the whole operation.

-- Each robot can collect 1 of its resource type per minute. It also takes one minute for the robot
-- factory (also conveniently from your pack) to construct any type of robot, although it consumes
-- the necessary resources available when construction begins.

-- Determine the largest number of geodes you could open using each of the first three blueprints.
-- What do you get if you multiply these numbers together?

import Text.ParserCombinators.Parsec (Parser, parse, many1, sepEndBy1, char, string, digit)
import System.Exit (exitFailure)
import Data.Maybe (maybe, fromMaybe, fromJust, isJust, catMaybes, mapMaybe)
import Data.List (foldl')
import qualified Data.Set as S
import qualified Data.Map.Strict as M

data Robot = Ore | Claw | Obsidian | Geode deriving (Eq, Ord, Show)
type Resource = Robot

type Resources = M.Map Resource Int
type Requrements = M.Map Robot Resources
type Factory = M.Map Robot Int

data Blueprint = Blueprint Int Requrements deriving (Eq, Show)
data State = State Blueprint Factory Resources deriving (Eq, Show)

type Instruction = [(Robot, Int)]

blueprintParser :: Parser Blueprint
blueprintParser = do 
  _ <- string "Blueprint "
  i <- numParser
  _ <- string ": Each ore robot costs "
  ore <- numParser
  _ <- string " ore. Each clay robot costs "
  claw <- numParser
  _ <- string " ore. Each obsidian robot costs "
  obsidian_ore <- numParser
  _ <- string " ore and "
  obsidian_claw <- numParser
  _ <- string " clay. Each geode robot costs "
  geode_ore <- numParser
  _ <- string " ore and "
  geode_obsidian <- numParser
  _ <- string " obsidian."
  return $ Blueprint i $ makeRequirements ore claw obsidian_ore obsidian_claw geode_ore geode_obsidian

numParser :: Parser Int
numParser = read <$> many1 digit

makeRequirements :: Int -> Int -> Int -> Int -> Int -> Int -> Requrements
makeRequirements ore claw obs_ore obs_claw g_ore g_obs = M.fromList $ map makeRequirements' r
  where
    makeRequirements' (k, v) = (k, M.fromList v)
    r = [
      (Ore, [(Ore, ore)]),
      (Claw, [(Ore, claw)]),
      (Obsidian, [(Ore, obs_ore), (Claw, obs_claw)]),
      (Geode, [(Ore, g_ore), (Obsidian, g_obs)])]

makeState :: Blueprint -> Int -> State
makeState b ore_num = State b (M.fromList [(Ore, ore_num)]) M.empty

req :: Robot -> State -> Resources
req r (State (Blueprint _ reqs) _ _) = fromJust $ M.lookup r reqs

factory :: State -> Factory
factory (State _ f _) = f

resources :: State -> Resources
resources (State _ _ r) = r

resource :: State -> Resource -> Int
resource s r = fromMaybe 0 $ M.lookup r $ resources s

buildRobot :: Robot -> State -> State
buildRobot robot s@(State b f r) = State b f' r'
  where
    f' = M.insertWith (+) robot 1 f
    r' = foldl' removeResource r $ M.toList $ req robot s
    removeResource acc (r, x) = M.insertWith (+) r (negate x) acc

addResources :: Int -> State -> State
addResources dt s@(State b f r) = State b f r'
  where
    production = M.toList $ factory s
    r' = foldl' addResource r production
    addResource acc (r, x) = M.insertWith (+) r (x * dt) acc

stepsToProduce :: Resource -> Int -> State -> Maybe Int
stepsToProduce r x state | not (r `M.member` factory state) = Nothing
stepsToProduce r x state = Just $ ceiling $ fromIntegral x / production r
  where
    production r = fromIntegral $ fromJust $ M.lookup r $ factory state

stepsToReach :: Resource -> Int -> State -> Maybe Int
stepsToReach r x state = stepsToReach' $ x - resource state r
  where
    stepsToReach' x | x <= 0 = Just 0
    stepsToReach' x = stepsToProduce r x state

stepsToMakeRobot :: Robot -> State -> Maybe Int
stepsToMakeRobot r state = predict $ map findSteps $ M.toList $ req r state
  where
    findSteps (r, x) = stepsToReach r x state
    predict steps | all isJust steps = Just $ (+ 1) $ maximum $ catMaybes steps
    predict _ = Nothing

possibleRobots :: State -> [(Robot, Int)]
possibleRobots state = mapMaybe steps robots
  where
    robots = Geode : filter isNotTooMany [Ore, Claw, Obsidian]
    isNotTooMany r@Ore = (5 >) $ fromMaybe 0 $ M.lookup r $ factory state
    isNotTooMany r@Claw = (10 >) $ fromMaybe 0 $ M.lookup r $ factory state
    isNotTooMany r = (20 >) $ fromMaybe 0 $ M.lookup r $ factory state
    isNotOverProduced = (18 >) . fromMaybe 0 . (`M.lookup` resources state)
    steps r = steps' r $ stepsToMakeRobot r state
    steps' r Nothing = Nothing
    steps' r (Just x) = Just (r, x)

possibleRobotsNoLaterThen :: State -> Int -> [(Robot, Int)]
possibleRobotsNoLaterThen state t = filter ((t >=) . snd) $ possibleRobots state

simulate :: State -> Int -> Int
simulate s t | t <= 1 = 0
simulate s t = forks robots
  where
    robots = possibleRobotsNoLaterThen s t
    score r@Geode dt = max 0 $! t - dt
    score _ dt = 0
    forks [] = 0
    forks ((r@Geode, dt@1):_) = makeFork (r, dt)
    forks robots = maximum $! map makeFork robots
    update r dt = buildRobot r $! addResources dt s
    makeFork (r, dt) = score r dt + simulate (update r dt) (t - dt)

total :: Int -> Blueprint -> Int
total t b = simulate (makeState b 1) t

parseInput :: Parser a -> IO a
parseInput parser = parseInput' parser >>= either report return
  where
    report err = do
      putStrLn $ "Error: " ++ show err
      exitFailure
    parseInput' p = do parse p "(stdin)" <$> getContents

main :: IO ()
main = do
  blueprints <- parseInput $ sepEndBy1 blueprintParser $ string "\n"
  print $ product $ map (total 32) $ take 3 blueprints
