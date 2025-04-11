module Repl (
    startRepl
) where

import Types
import Errors
import Parser
import Interpreter
import MyMap
import System.IO (hFlush, stdout)

type EvalState = (Stack, Dictionary)

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
        ":stack" -> printStack state >> replLoop state -- Prints current stack, for debugging
        ":map" -> printDictionary state >> replLoop state
        _ -> processInput input state >>= replLoop

processInput :: String -> EvalState -> IO EvalState
processInput input state = 
    case parseTokens $ tokenizer input of
        Left err -> do
            putStrLn $ prettyErr err
            return state
        Right program ->
            case evalProgram program state of
                Left err -> do
                    putStrLn $ prettyErr err
                    return state
                Right newState -> do
                    return newState

printStack :: EvalState -> IO ()
printStack (stk,_) = putStrLn $ "Stack: " ++ show stk

printDictionary :: EvalState -> IO ()
printDictionary (_,eval) = putStrLn $ "Directory: " ++ show eval

