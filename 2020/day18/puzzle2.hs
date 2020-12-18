#! /usr/bin/env nix-shell
#! nix-shell -i runhaskell -p

-- https://adventofcode.com/2020/day/18#part2
-- You manage to answer the child's questions and they finish part 1 of
-- their homework, but get stuck when they reach the next section: advanced math.

-- Now, addition and multiplication have different precedence levels, but they're
-- not the ones you're familiar with. Instead, addition is evaluated before multiplication.

-- What do you get if you add up the results of evaluating the homework problems
-- using these new rules?

import qualified Text.ParserCombinators.Parsec.Token as P
import Text.ParserCombinators.Parsec (Parser, (<|>), parse, many)
import Text.Parsec.Language (emptyDef)
import System.Exit (exitFailure)

data Token = Number Integer | Fun Operator | Parentheses [Token] deriving (Show, Eq, Ord)
data Operator = Sum | Mult deriving (Show, Eq, Ord)

tokensParser :: Parser [Token]
tokensParser = many tokenParser

tokenParser :: Parser Token
tokenParser = numberParser <|> funParser <|> parenthesesParser

numberParser :: Parser Token
numberParser = do
  num <- P.natural lexer
  return $ Number num

funParser :: Parser Token
funParser = sumParser <|> multParser

sumParser :: Parser Token
sumParser = do
  _ <- P.symbol lexer "+"
  return $ Fun Sum

multParser :: Parser Token
multParser = do
  _ <- P.symbol lexer "*"
  return $ Fun Mult

parenthesesParser :: Parser Token
parenthesesParser = do
  tokens <- P.parens lexer tokensParser
  return $ Parentheses tokens

lexer = P.makeTokenParser emptyDef

calculate :: [Token] -> Integer
calculate = head . foldl calculate' []
  where
    calculate' stack (Number n) = n : stack
    calculate' (n1:n2:stack) (Fun op) = (applyOp op n1 n2) : stack
    calculate' _ _ = error "oops"

buildRPN :: [Token] -> [Token]
buildRPN = buildRPN' []
  where
    buildRPN' stack [] = map Fun stack
    buildRPN' stack ((Number n):xs) = (Number n) : (buildRPN' stack xs)
    buildRPN' stack ((Parentheses ts):xs) = buildRPN ts ++ buildRPN' stack xs
    buildRPN' stack ((Fun op):xs) = (reverse newResult) ++ buildRPN' (op:newStack) xs
      where
        newStack = dropWhile (isPriorityLower op) stack
        newResult = map Fun $ takeWhile (isPriorityLower op) stack

applyOp :: Operator -> Integer -> Integer -> Integer
applyOp Sum = (+)
applyOp Mult = (*)

priority :: Operator -> Int
priority Sum = 1
priority Mult = 2

isPriorityLower :: Operator -> Operator -> Bool
isPriorityLower op1 op2 = (priority op1) > (priority op2)

parseInputLines :: Parser a -> IO [a]
parseInputLines parser = parseInput' parser >>= either report return
  where
    report err = do
      putStrLn $ "Errors: " ++ show err
      exitFailure
    parseInput' p = do
      input <- getContents
      return $ mapM (parse p "(stdin)") $ lines input

main :: IO ()
main = do
  expressions <- parseInputLines tokensParser
  putStrLn $ show $ sum $ map (calculate . buildRPN) expressions
