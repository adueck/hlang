module Main
  ( interp,
    main,
  )
where

import Control.Monad.State.Lazy
import Data.Map qualified as Map
import Eval
import Parser
import System.Environment
import Types

main = do
  args <- getArgs
  if null args
    then do
      putStrLn "Welcome to HParser"
      putStrLn "Enter an expression or type 'exit' to quit"
      runPrompt
    else do
      contents <- readFile (head args)
      interp contents

runPrompt = do
  putStr "> "
  l <- getLine
  unless (l == "exit") $ do
    interp l
    runPrompt

interp :: String -> IO ()
interp l = do
  let pr = runParser parseSExpr l
  case pr of
    Left (s, ls) -> do
      let dist = length l - length ls
      putStrLn $ replicate (dist + 1) ' ' ++ "^"
      putStrLn $ "SYNTAX ERROR: " ++ s ++ " at char " ++ show dist
    Right (s, ls) -> do
      case runState (evalSExpr s) Map.empty of
        (Left err, env) -> putStrLn $ "RUNTIME ERROR: " ++ err
        (Right i, env) -> print (i, env)
