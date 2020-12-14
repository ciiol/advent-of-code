#! /usr/bin/env nix-shell
#! nix-shell -i runhaskell -p

-- https://adventofcode.com/2020/day/14
-- A version 2 decoder chip doesn't modify the values being written at all.
-- Instead, it acts as a memory address decoder. Immediately before a value
-- is written to memory, each bit in the bitmask modifies the corresponding
-- bit of the destination memory address in the following way:

--     If the bitmask bit is 0, the corresponding memory address bit is unchanged.
--     If the bitmask bit is 1, the corresponding memory address bit is overwritten with 1.
--     If the bitmask bit is X, the corresponding memory address bit is floating.

-- A floating bit is not connected to anything and instead fluctuates unpredictably.
-- In practice, this means the floating bits will take on all possible values,
-- potentially causing many memory addresses to be written all at once!

import qualified Text.ParserCombinators.Parsec.Token as P
import qualified Data.Bits as B
import qualified Data.Map as M
import Data.Bits ((.|.))
import Data.Word (Word64)
import Text.ParserCombinators.Parsec (Parser, parse, many, try, (<|>), digit, char)
import Text.Parsec.Language (emptyDef)
import System.Exit (exitFailure)

data Operation = SetMask Mask | Write Address MemoryValue deriving (Show, Eq, Ord)
data Mask = Mask {mOnes :: MaskField, mFloating :: MaskField} deriving (Show, Eq, Ord)
type MaskField = Word64
type MemoryValue = Word64
type Address = Word64
type Memory = M.Map Address MemoryValue
data Computer = Computer {cMemory :: Memory, cMask :: Mask} deriving (Show, Eq, Ord)

operationsParser :: Parser [Operation]
operationsParser = many operationParser

operationParser :: Parser Operation
operationParser = maskParser <|> writeParser

maskParser :: Parser Operation
maskParser = do
  _ <- try $ P.symbol lexer "mask"
  _ <- P.symbol lexer "="
  arg <- P.lexeme lexer $ many $ digit <|> char 'X'
  return $ SetMask $ makeMask arg

writeParser :: Parser Operation
writeParser = do
  _ <- try $ P.symbol lexer "mem"
  address <- P.brackets lexer (P.integer lexer)
  _ <- P.symbol lexer "="
  value <- P.integer lexer
  return $ Write (fromIntegral address) (fromIntegral value)

lexer = P.makeTokenParser emptyDef

makeMask :: [Char] -> Mask
makeMask = makeMask' 0 0
  where
    makeMask' accf acc1 [] = Mask {mFloating = accf, mOnes = acc1}
    makeMask' accf acc1 ('0':xs) = makeMask' (accf * 2) (acc1 * 2) xs
    makeMask' accf acc1 ('1':xs) = makeMask' (accf * 2) (acc1 * 2 + 1) xs
    makeMask' accf acc1 ('X':xs) = makeMask' (accf * 2 + 1) (acc1 * 2) xs
    makeMask' _accf _acc1 (x:_xs) = error $ "unexpected token " ++ show x

applyMask :: Mask -> Address -> [Address]
applyMask mask address = unfloat (mFloating mask) $ address .|. (mOnes mask)

unfloat :: MaskField -> Address -> [Address]
unfloat field address = floatings bits
  where
    multiply i n = [B.setBit n i, B.clearBit n i]
    floatings [] = [address]
    floatings (x:xs) = concat $ map (multiply x) $ floatings xs
    bits = filter (B.testBit field) $ take (B.finiteBitSize field) [0..]

makeComputer :: Computer
makeComputer = Computer M.empty $ makeMask []

applyOperation :: Computer -> Operation -> Computer
applyOperation c (SetMask mask) = c {cMask = mask}
applyOperation c (Write address value) = c {cMemory = newMemory}
  where
    newMemory = foldl setMemory (cMemory c) addresses
    addresses = applyMask (cMask c) address
    setMemory m a = M.insert a value m

allMemoryValues :: Computer -> [MemoryValue]
allMemoryValues = map snd . M.toList . cMemory

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
  operations <- parseInput operationsParser
  putStrLn $ show $ sum $ allMemoryValues $ foldl applyOperation makeComputer operations
