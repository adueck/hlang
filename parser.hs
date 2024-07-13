{-# LANGUAGE OverlappingInstances #-}

module AParser where

import Control.Applicative
import Control.Monad.State.Lazy
import Data.Char
import Data.Map qualified as Map

--------
-- TYPES
--------

newtype Parser a = Parser {runParser :: String -> ParseResult a}

type ParseResult a = Either Fail (ParseSuccess a)

type Fail = (String, String)

-- | The parse value, the string remaining,
-- and the char number of the parsed value
type ParseSuccess a = (a, String)

instance Functor Parser where
  fmap :: (a -> b) -> Parser a -> Parser b
  fmap f (Parser rp) = Parser g
    where
      g xs =
        let res = rp xs
         in case res of
              Left a -> Left a
              Right (val, rem) -> Right (f val, rem)

instance Applicative Parser where
  pure :: a -> Parser a
  pure a = Parser g
    where
      g xs = Right (a, xs)
  (<*>) :: Parser (a -> b) -> Parser a -> Parser b
  (Parser rp1) <*> (Parser rp2) = Parser f
    where
      f xs = case rp1 xs of
        Left e -> Left e
        Right (pf, s') -> g s'
          where
            g xs' = case rp2 xs' of
              Left e -> Left e
              Right (val, l) -> Right (pf val, l)

instance Alternative (Either Fail) where
  empty :: Either Fail a
  empty = Left ("", "")
  (<|>) :: Either Fail a -> Either Fail a -> Either Fail a
  (Right a) <|> (Right _) = Right a
  (Left _) <|> (Right a) = Right a
  (Right a) <|> (Left _) = Right a
  (Left (errA, rA)) <|> (Left (errB, rB)) =
    let aLength = length rA
        bLength = length rB
     in if aLength == bLength
          then Left (errA ++ " OR " ++ errB, rA)
          else
            if bLength < aLength
              then Left (errB, rB)
              else Left (errA, rA)

instance Alternative Parser where
  empty :: Parser a
  empty = Parser $ pure (Left ("", ""))
  (<|>) :: Parser a -> Parser a -> Parser a
  (Parser rp1) <|> (Parser rp2) = Parser f
    where
      f xs = rp1 xs <|> rp2 xs

instance Monad Parser where
  (>>=) :: Parser a -> (a -> Parser b) -> Parser b
  p >>= f = Parser $ \s -> case runParser p s of
    Left a -> Left a
    Right (v, s') -> runParser (f v) s'

-- let res2 = runParser (f v) s'
--  in case res2 of
--       Left (err, p') -> Left (err, p')
--       Right (v', s'') -> Right (v', s'')

data Operation = Add | Multiply | Subtract deriving (Eq, Show)

type Identifier = String

-- | An "atom" is either an integer value or on operation primitive.
data Atom = N Integer | O Operation | I Identifier deriving (Eq, Show)

-- | An S-expression is either an atom, or a list of S-expressions.
data SExpr
  = A Atom
  | Comb [SExpr]
  deriving (Eq, Show)

-- try :: Parser a -> Parser a
-- try p = Parser f
--   where
--     f s = case runParser p s of
--       Left (s, i, _) -> Left (s, i, 0)
--       x -> x

----------
-- PARSING
----------

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
  spaces
    *> ( (A <$> parseAtom)
           <|> (Comb <$> parseComb)
       ) -- add msg thing here
    <* spaces

-------------
-- EVALUATING
-------------

type Value = Integer

type EvalResult = Either String Value

type Env = Map.Map Identifier Value

type EvalState = State Env EvalResult

evalSExpr :: SExpr -> EvalState
evalSExpr s = case s of
  A a -> evalAtom a
  Comb ss -> evalComb ss

evalAtom :: Atom -> EvalState
evalAtom a = case a of
  N i -> return $ Right i
  O i -> return $ Left "number value expected"
  I i -> evalIdent i

evalIdent :: Identifier -> EvalState
evalIdent i = do
  env <- get
  case Map.lookup i env of
    Nothing -> return $ Left $ "undefined variable " ++ i
    Just v -> return $ Right v

evalOpExp :: Operation -> [SExpr] -> EvalState
evalOpExp Add = sumUpXs
evalOpExp Subtract = minusUpXs
evalOpExp Multiply = multiplyXs

evalIdentExp :: Identifier -> [SExpr] -> EvalState
evalIdentExp i s
  | i == "let" = evalLet s
  | otherwise = return $ Left "custom functions not implemented"

evalLet :: [SExpr] -> EvalState
evalLet [def, body] = case def of
  Comb [var, val] -> case var of
    A (I i) -> do
      env <- get
      res <- evalSExpr val
      case res of
        Left err -> return $ Left err
        Right v -> do
          put $ Map.insert i v env
          evalSExpr body
    _ -> return $ Left "invalid let body"
  _ -> return $ Left "invalid let body, definition required"
evalLet _ = return $ Left "invalid let body"

evalComb :: [SExpr] -> EvalState
evalComb [] = return $ Left "Empty S-Expression"
evalComb (x : xs) = case x of
  Comb s -> return $ Left "Operator missing"
  A s -> case s of
    N y -> return $ Left "Found number value, Operator required"
    O o -> evalOpExp o xs
    I i -> evalIdentExp i xs

monoidOp :: (Integer -> Integer -> Integer) -> Integer -> [SExpr] -> EvalState
monoidOp f base [] = return $ Right base
monoidOp f base (x : xs) = state g
  where
    g s = case runState (evalSExpr x) s of
      (Left a, env) -> (Left a, env)
      (Right i, env) -> case runState (monoidOp f base xs) env of
        (Left a, env) -> (Left a, env)
        (Right v, env) -> (Right (f v i), env)

sumUpXs :: [SExpr] -> EvalState
sumUpXs = monoidOp (+) 0

multiplyXs :: [SExpr] -> EvalState
multiplyXs = monoidOp (*) 1

minusUpXs :: [SExpr] -> EvalState
minusUpXs [] = return $ Left "expression missing"
minusUpXs [x] = state f
  where
    f s = case runState (evalSExpr x) s of
      (Left a, env) -> (Left a, env)
      (Right i, env) -> (Right (-i), env)
minusUpXs (x : xs) = state f
  where
    f s = case runState (evalSExpr x) s of
      (Left a, env) -> (Left a, env)
      (Right toStart, env) -> case runState (sumUpXs xs) env of
        (Left a, env) -> (Left a, env)
        (Right toSub, env) -> (Right (toStart - toSub), env)

---------------
-- EVAL LOOP IO
---------------

main = do
  putStrLn "Welcome to AParser"
  putStrLn "Enter an expression or type 'exit' to quit"
  runPrompt

runPrompt = do
  putStr "> "
  l <- getLine
  unless (l == "exit") $ do
    let pr = runParser parseSExpr l
    case pr of
      Left (s, ls) -> do
        let dist = length l - length ls
        putStrLn $ replicate (dist + 1) ' ' ++ "^"
        putStrLn $ "SYNTAX ERROR: " ++ s ++ " at char " ++ show dist
      Right (s, ls) -> do
        case runState (evalSExpr s) Map.empty of
          (Left err, env) -> putStrLn $ "RUNTIME ERROR: " ++ err
          (Right i, env) -> print i
    runPrompt
