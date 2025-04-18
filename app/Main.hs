module Main (main) where

import ReplMode
import NormalMode
import System.Environment (getArgs)

-- Evaluates to run NORMAL mode or REPL mode
main :: IO ()
main = do
    args <- getArgs
    case args of
        [] -> startRepl
        [file] -> runNormal file
        _ -> putStrLn "Wrong usage"




