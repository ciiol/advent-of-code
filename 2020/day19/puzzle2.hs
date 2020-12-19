#! /usr/bin/env nix-shell
#! nix-shell -i runhaskell -p

-- https://adventofcode.com/2020/day/19
-- As you look over the list of messages, you realize your matching rules aren't quite right.
-- To fix them, completely replace rules 8: 42 and 11: 42 31 with the following:

-- 8: 42 | 42 8
-- 11: 42 31 | 42 11 31

-- After updating rules 8 and 11, how many messages completely match rule 0?


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
  res <- try $ mapM (try . flip defRefParser index) refs
  return $ concat res

defConstantParser :: Char -> Parser String
defConstantParser c = do
  r <- char c
  return [r]

defOneOfParser :: [[Integer]] -> RuleIndex -> Parser String
defOneOfParser [] _index = error "oops"
defOneOfParser [s] index = defSeqParser s index
defOneOfParser (s:ss) index = do
  res <- (defSeqParser s index) <|> (defOneOfParser ss index)
  return res

deinitionParser :: RuleDefinition -> RuleIndex -> Parser String
deinitionParser (Constant c) _index = defConstantParser c
deinitionParser (OneOf s) index = defOneOfParser s index

defRefParser :: Integer -> RuleIndex -> Parser String
defRefParser i index = deinitionParser (index M.! i) index

buildChecker :: RuleIndex -> Parser Bool
buildChecker index = do
  prefix <- many1 $ defRefParser 42 index
  suffix <- many1 $ defRefParser 31 index
  _ <- eof
  return $ (length suffix) < (length prefix)

applyChecker :: Parser Bool -> String -> Bool
applyChecker p = handle . parse p ""
  where
    handle (Left _err) = False
    handle (Right res) = res

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
  putStrLn $ show $ length $ filter (applyChecker $ buildChecker $ ruleIndex $ iRules input) $ iMessages input
