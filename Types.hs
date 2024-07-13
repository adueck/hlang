module Types
  ( Parser (..),
    ParseResult,
    Fail,
    ParseSuccess,
    Operation (..),
    Identifier,
    Atom (..),
    SExpr (..),
  )
where

import Control.Applicative

-- PARSER TYPES

newtype Parser a = Parser {runParser :: String -> ParseResult a}

type ParseResult a = Either Fail (ParseSuccess a)

type Fail = (String, String)

-- | The parse value, the string remaining,
-- and the char number of the parsed value
type ParseSuccess a = (a, String)

-- EXPRESSION TYPES

data Operation = Add | Multiply | Subtract deriving (Eq, Show)

type Identifier = String

-- | An "atom" is either an integer value or on operation primitive.
data Atom = N Integer | O Operation | I Identifier deriving (Eq, Show)

-- | An S-expression is either an atom, or a list of S-expressions.
data SExpr
  = A Atom
  | Comb [SExpr]
  deriving (Eq, Show)

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