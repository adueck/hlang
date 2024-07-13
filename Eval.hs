module Eval
  ( evalSExpr,
  )
where

import Control.Applicative
import Control.Monad.State.Lazy
import Data.Map qualified as Map
import Types

data Value = Intg Integer | Func Function deriving (Show, Eq)

data Function = Function Identifier SExpr Env deriving (Show, Eq)

type EvalResult = Either String Value

type Env = Map.Map Identifier Value

type EvalState = State Env EvalResult

evalSExpr :: SExpr -> EvalState
evalSExpr s = case s of
  A a -> evalAtom a
  Comb ss -> evalComb ss

evalAtom :: Atom -> EvalState
evalAtom a = case a of
  N i -> return $ Right $ Intg i
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
evalLet [def, body] = do
  case def of
    Comb defs -> do
      mapM_ addDef defs
      return $ Right 0
    _ -> return $ Left "list of definitions required in let definitions section"
  evalSExpr body
evalLet _ = return $ Left "invalid let expression"

evalLambda :: [SExpr] -> EvalState
evalLambda [param, body] = do
  case param of
    (A (I p)) -> do
      env <- get
      return $ Right $ Func $ Function p body env
    _ -> return $ Left "One param required for lambda expression"
evalLambda _ = return $ Left "invalid lambda expression"

applyLambda :: Function -> SExpr -> EvalState
applyLambda (Function param body env) arg = do
  basicEnv <- get
  let valR = runState (evalSExpr arg) basicEnv
  case valR of
    (Right val, env') -> do
      let env'' = Map.union env' basicEnv
      let withParam = Map.insert param val env''
      put withParam
      evalSExpr body
    _ -> return $ Left "error evaluating argument for lambda"

addDef :: SExpr -> EvalState
addDef s = case s of
  Comb [var, val] -> case var of
    A (I i) -> do
      env <- get
      res <- evalSExpr val
      case res of
        Left err -> return $ Left err
        Right v -> do
          put $ Map.insert i v env
          return $ Right $ Intg 0
    _ -> return $ Left "invalid let body"
  _ -> return $ Left "invalid let body, definition required"

evalComb :: [SExpr] -> EvalState
evalComb [] = return $ Left "Empty S-Expression"
evalComb (x : xs) = case x of
  (A (O o)) -> evalOpExp o xs
  (A (I "lambda")) -> evalLambda xs
  (A (I "let")) -> evalLet xs
  _ -> do
    env <- get
    case runState (evalSExpr x) env of
      (Right (Func f), env) -> applyLambda f (head xs)
      a -> return $ Left (show a)

monoidOp :: (Integer -> Integer -> Integer) -> Integer -> [SExpr] -> EvalState
monoidOp f base [] = return $ Right (Intg base)
monoidOp f base (x : xs) = state g
  where
    g s = case runState (evalSExpr x) s of
      (Left a, env) -> (Left a, env)
      (Right i, env) -> case runState (monoidOp f base xs) s of
        (Left a, env) -> (Left a, env)
        (Right v, env) -> case (v, i) of
          (Intg val, Intg int) -> (Right (Intg (f val int)), env)
          _ -> (Left "interger value(e) neede", env)

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
      (Right val, env) -> case val of
        Intg i -> (Right $ Intg (-i), env)
        _ -> (Left "Integer value(s) needed", env)
minusUpXs (x : xs) = state f
  where
    f s = case runState (evalSExpr x) s of
      (Left a, env) -> (Left a, env)
      (Right toStart, env) -> case runState (sumUpXs xs) s of
        (Left a, env) -> (Left a, env)
        (Right toSub, env) -> case (toStart, toSub) of
          (Intg iToStart, Intg iToSub) -> (Right $ Intg (iToStart - iToSub), env)
          _ -> (Left "Integer value(s) needed", env)
