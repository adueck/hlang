module Interp where

import Data.List
import Main qualified as M
import System.Environment

main = do
  args <- getArgs
  contents <- readFile (head args)
  M.interp contents