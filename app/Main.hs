module Main (main) where

-- Internal libs
import Bprog.ReplMode
import Bprog.NormalMode

-- external libs
import System.Environment (getArgs)

-- Evaluates to run NORMAL mode or REPL mode
main :: IO ()
main = do
    args <- getArgs
    case args of
        [] -> startRepl
        [file] -> runNormal file
        _ -> putStrLn "Wrong usage"




