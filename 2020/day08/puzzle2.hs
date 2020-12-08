#! /usr/bin/env nix-shell
#! nix-shell -i runhaskell -p

-- https://adventofcode.com/2020/day/8#part2
-- The program is supposed to terminate by attempting to execute
-- an instruction immediately after the last instruction in the file.

-- By changing exactly one jmp or nop, you can repair the
-- boot code and make it terminate correctly.

-- What is the value of the accumulator after the program terminates?

import qualified Data.Map as M
import qualified Text.ParserCombinators.Parsec.Token as P
import Data.List (find)
import Text.ParserCombinators.Parsec (Parser, (<|>), try, many, parse)
import Text.Parsec.Language (emptyDef)
import System.Exit (exitFailure)

data Instruction = Acc (Int) | Jmp (Int) | Nop (Int) deriving (Show, Eq, Ord)
type Code = [Instruction]
type Memory = Int
data Computer = Computer
  {
    cIPoiner :: Int,
    cMemory :: Memory,
    cCode :: M.Map Int Instruction
  }
  deriving (Show, Eq, Ord)

codeParser :: Parser Code
codeParser = many instructionParser

instructionParser :: Parser Instruction
instructionParser = jmpParser <|> accParser <|> nopParser

jmpParser :: Parser Instruction
jmpParser = do
  _ <- try $ P.symbol lexer "jmp"
  arg <- P.integer lexer
  return $ Jmp $ fromIntegral arg

accParser :: Parser Instruction
accParser = do
  _ <- try $ P.symbol lexer "acc"
  arg <- P.integer lexer
  return $ Acc $ fromIntegral arg

nopParser :: Parser Instruction
nopParser = do
  _ <- try $ P.symbol lexer "nop"
  arg <- P.integer lexer
  return $ Nop $ fromIntegral arg

lexer = P.makeTokenParser emptyDef

makeComputer :: Code -> Computer
makeComputer code = Computer 0 0 $ M.fromList $ enumerate code

updateMemory :: (Memory -> Memory) -> Computer -> Computer
updateMemory f (Computer i m c) = Computer i (f m) c

updateIPointer :: (Int -> Int) -> Computer -> Computer
updateIPointer f (Computer i m c) = Computer (f i) m c

applyInstruction :: Instruction -> Computer -> Computer
applyInstruction (Acc num) = updateIPointer (+ 1) . updateMemory (+ num)
applyInstruction (Jmp num) = updateIPointer (+ num)
applyInstruction (Nop _) = updateIPointer (+ 1)

enumerate :: [a] -> [(Int, a)]
enumerate = zip [0..]

nextInstruction :: Computer -> Maybe Instruction
nextInstruction computer = M.lookup (cIPoiner computer) (cCode computer)

trace :: Computer -> [Computer]
trace computer = computer : traceTail
  where
    traceTail = maybe [] traceApply $ nextInstruction computer
    traceApply i = trace $ applyInstruction i computer

thinOut :: [a] -> [a]
thinOut [] = []
thinOut (_x:xs) = thinOut' xs
  where
    thinOut' (x':xs') = x' : (thinOut xs')
    thinOut' [] = []

isCycled :: [Computer] -> Bool
isCycled computers = any sameIP $ zip computers $ thinOut computers
  where
    sameIP (c1, c2) = (cIPoiner c1) == (cIPoiner c2)

flipInstruction :: Instruction -> [Instruction]
flipInstruction (Jmp num) = [(Nop num)]
flipInstruction (Nop num) = [(Jmp num)]
flipInstruction _ = []

fixes :: Code -> [Code]
fixes [] = []
fixes (x:xs) = (addHeads other x) ++ (map (flip (:) xs) $ flipInstruction x)
  where
    other = fixes xs
    addHeads fs h = map ((:) h) fs

fixedCode :: Code -> Code
fixedCode code = maybe (error "unfixabel") id $ find isUnCycledCode $ fixes code
  where
    isUnCycledCode fixed = not $ isCycled $ trace $ makeComputer fixed

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
  code <- parseInput codeParser
  putStrLn $ show $ cMemory $ last $ trace $ makeComputer $ fixedCode code
