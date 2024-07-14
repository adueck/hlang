module Parser
  ( parseSExpr,
  )
where

import Control.Applicative
import Data.Char
import Types

satisfy :: (Char -> Bool) -> Parser Char
satisfy p = Parser f
  where
    f [] = Left ("Unexpected end of input", [])
    f (x : xs)
      | p x = Right (x, xs)
      | otherwise = Left ("Parsing error on " ++ [x], xs)

char :: Char -> Parser Char
char c = satisfy (== c)

string :: String -> Parser String
string "" = return ""
string (x : xs) = (:) <$> char x <*> string xs

isEnd :: Parser Char
isEnd = Parser f
  where
    f [] = Right ('a', [])
    f xs = Left ("not end", xs)

comment :: Parser [Char]
comment = (char '#' *> manyTill (char '\n' <|> isEnd) anything) <|> pure []

anything :: Parser Char
anything = satisfy (const True)

posInt :: Parser Integer
posInt = Parser f
  where
    f xs
      | null ns = Left ("Integer expected", xs)
      | otherwise = Right (read ns, rest)
      where
        (ns, rest) = span isDigit xs

zeroOrMore :: Parser a -> Parser [a]
zeroOrMore p = oneOrMore p <|> pure []

oneOrMore :: Parser a -> Parser [a]
oneOrMore p = (:) <$> p <*> zeroOrMore p

whiteSpace :: Parser [Char]
whiteSpace = (++) <$> spaces <*> ((++) <$> comment <*> spaces)

spaces :: Parser String
spaces = zeroOrMore (satisfy isSpace)

parseAtom :: Parser Atom
parseAtom = (O <$> parseOp) <|> (N <$> posInt) <|> (I <$> parseIdentifier)

parseOp :: Parser Operation
parseOp = Parser f
  where
    f [] = Left ("Unexpected end of input", [])
    f (x : xs)
      | x == '+' = Right (Add, xs)
      | x == '-' = Right (Subtract, xs)
      | x == '*' = Right (Multiply, xs)
      | otherwise = Left ("Operator expected", xs)

parseIdentifier :: Parser Identifier
parseIdentifier = (:) <$> satisfy isAlpha <*> zeroOrMore (satisfy isAlphaNum)

manyTill :: Parser a -> Parser b -> Parser [b]
manyTill pEnd p = Parser f
  where
    f xs = case runParser pEnd xs of
      Right (_, xs') -> Right ([], xs')
      Left a -> runParser ((:) <$> p <*> manyTill pEnd p) xs

parseComb :: Parser [SExpr]
parseComb =
  char '('
    *> manyTill (char ')') parseSExpr

parseSExpr :: Parser SExpr
parseSExpr =
  whiteSpace
    *> ( (A <$> parseAtom)
           <|> (Comb <$> parseComb)
       ) -- add msg thing here
    <* whiteSpace
