module Repl (
    startRepl
) where

import Types
import Errors
import Parser
import Interpreter
import MyMap
import System.IO (hFlush, stdout)

startRepl :: IO ()
startRepl = do
    putStrLn "Welcome to the BPROG REPL"
    replLoop ([],myEmpty) -- initilze the dictionary to be empty

replLoop :: EvalState -> IO ()
replLoop state = do
    putStr "bprog> "
    hFlush stdout
    input <- getLine
    case input of
        ":q" -> putStrLn "Goodbye!"
        ":quit" -> putStrLn "Goodbye!"
        ":s" -> printStack state >> replLoop state
        ":stack" -> printStack state >> replLoop state
        ":m" -> printDictionary state >> replLoop state
        ":map" -> printDictionary state >> replLoop state
        _ -> processInput input state >>= replLoop 

processInput :: String -> EvalState -> IO EvalState
processInput input state = do
    let toks = tokenizer input
    case parseTokens toks of
        Left err -> (putStrLn $ prettyErr err) >> pure state
        Right program -> do
            result <- evalProgram program state
            case result of
                Left err -> (putStrLn $ prettyErr err) >> pure state
                Right newState -> pure newState


-- Helper functions to print out stack and dictionary

printStack :: EvalState -> IO ()
printStack (stk,_) = putStrLn $ "Stack: " ++ show stk

printDictionary :: EvalState -> IO ()
printDictionary (_,eval) = putStrLn $ "Map: " ++ show eval



