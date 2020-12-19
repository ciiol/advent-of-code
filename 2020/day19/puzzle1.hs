#! /usr/bin/env nix-shell
#! nix-shell -i runhaskell -p

-- https://adventofcode.com/2020/day/19
-- They sent you a list of the rules valid messages should obey and a list of received messages they've collected so far (your puzzle input).

-- How many messages completely match rule 0?

import qualified Text.ParserCombinators.Parsec.Token as P
import qualified Data.Map.Strict as M
import Text.ParserCombinators.Parsec (Parser, (<|>), parse, endBy, sepBy1, newline, alphaNum, char, try, many1, eof, digit, many)
import Text.Parsec.Language (emptyDef)
import System.Exit (exitFailure)

data Rule = Rule {rID :: Integer, rDef :: RuleDefinition} deriving (Show, Eq, Ord)
data Input = Input {iRules :: [Rule], iMessages :: [String]} deriving (Show, Eq, Ord)
data RuleDefinition = Constant Char | OneOf [[Integer]] deriving (Show, Eq, Ord)
type RuleIndex = M.Map Integer RuleDefinition

rulesParser :: Parser [Rule]
rulesParser = endBy ruleParser newline

ruleParser :: Parser Rule
ruleParser = do
  ruleID <- numberParser
  _ <- P.symbol lexer ":"
  def <- definitionParser
  return $ Rule ruleID def

definitionParser :: Parser RuleDefinition
definitionParser = constantParser <|> oneOfParser

constantParser :: Parser RuleDefinition
constantParser = do
  _ <- char '"'
  c <- alphaNum
  _ <- char '"'
  return $ Constant c

oneOfParser :: Parser RuleDefinition
oneOfParser = do
  def <- sepBy1 (many1 numberParser) $ P.symbol lexer "|"
  return $ OneOf def

messagesParser :: Parser [String]
messagesParser = endBy (many1 alphaNum) newline

inputParser :: Parser Input
inputParser = do
  rules <- rulesParser
  _ <- newline
  messages <- messagesParser
  return $ Input rules messages

numberParser :: Parser Integer
numberParser = do
  s <- many1 digit
  _ <- many $ char ' '
  return $ read s

lexer = P.makeTokenParser emptyDef

ruleIndex :: [Rule] -> RuleIndex
ruleIndex = M.fromList . map prepare
  where
    prepare r = (rID r, rDef r)

defSeqParser :: [Integer] -> RuleIndex -> Parser String
defSeqParser refs index = do
  res <- try $ mapM (flip defRefParser index) refs
  return $ concat res

defConstantParser :: Char -> Parser String
defConstantParser c = do
  r <- char c
  return $ [r]

defOneOfParser :: [[Integer]] -> RuleIndex -> Parser String
defOneOfParser [] _index = error "oops"
defOneOfParser [s] index = defSeqParser s index
defOneOfParser (s:ss) index = do
  res <- (defSeqParser s index) <|> (defOneOfParser ss index)
  return $ res

deinitionParser :: RuleDefinition -> RuleIndex -> Parser String
deinitionParser (Constant c) _index = defConstantParser c
deinitionParser (OneOf s) index = defOneOfParser s index

defRefParser :: Integer -> RuleIndex -> Parser String
defRefParser i index = deinitionParser (index M.! i) index

buildParser :: [Rule] -> Parser String
buildParser rules = do
  res <- defRefParser 0 $ ruleIndex rules
  _ <- eof
  return $ res

isParsable :: Parser a -> String -> Bool
isParsable p = handle . parse p ""
  where
    handle (Left _err) = False
    handle (Right _res) = True

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
  input <- parseInput inputParser
  putStrLn $ show $ length $ filter (isParsable $ buildParser $ iRules input) $ iMessages input
