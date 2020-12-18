#! /usr/bin/env nix-shell
#! nix-shell -i runhaskell -p

-- https://adventofcode.com/2020/day/18
-- The homework (your puzzle input) consists of a series of expressions
-- that consist of addition (+), multiplication (*), and parentheses ((...)).
-- Just like normal math, parentheses indicate that the expression inside
-- must be evaluated before it can be used by the surrounding expression.
-- Addition still finds the sum of the numbers on both sides of the operator,
-- and multiplication still finds the product.

-- However, the rules of operator precedence have changed. Rather than evaluating
-- multiplication before addition, the operators have the same precedence, and are
-- evaluated left-to-right regardless of the order in which they appear.

-- Evaluate the expression on each line of the homework; what is the sum
-- of the resulting values?

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

calculateOne :: [Token] -> Integer
calculateOne = handle . calculate
  where
    handle (x, []) = x
    handle _ = error "more then one"

calculateAll :: [Token] -> [Integer]
calculateAll [] = []
calculateAll tokens = (fst result) : (calculateAll $ snd result)
  where
    result = calculate tokens

calculate :: [Token] -> (Integer, [Token])
calculate (expr1:(Fun op):expr2:xs) = calculate $ (Number $ applyOp op (reduce expr1) (reduce expr2)):xs
calculate (expr:xs) = ((reduce expr), xs)
calculate tokens = error $ "can't calculate " ++ (show tokens)

reduce :: Token -> Integer
reduce (Number n) = n
reduce (Parentheses ts) = calculateOne ts
reduce _ = error "can't reduce"

applyOp :: Operator -> Integer -> Integer -> Integer
applyOp Sum = (+)
applyOp Mult = (*)

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
  tokens <- parseInput tokensParser
  putStrLn $ show $ sum $ calculateAll tokens
