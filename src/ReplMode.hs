module ReplMode (
    startRepl
) where

import Types
import Errors
import Parser
import Interpreter
import BprogIO
import System.IO (hFlush, stdout)
import qualified Data.Map as Map


startRepl :: IO ()
startRepl = do
    putStrLn "Welcome to the BPROG REPL"
    replLoop ([],Map.empty) -- initilze the stack and dictionary to be empty

replLoop :: EvalState -> IO ()
replLoop state = do
    putStr "bprog> "
    hFlush stdout
    input <- getLine
    case input of
        -- Exit loop
        ":q" -> putStrLn "Goodbye!"
        ":quit" -> putStrLn "Goodbye!"
        -- Keep old state
        ":s" -> printStack state >> replLoop state
        ":stack" -> printStack state >> replLoop state
        ":m" -> printDictionary state >> replLoop state
        ":map" -> printDictionary state >> replLoop state
        -- Make a new State
        _ -> processInput input state >>= replLoop 

processInput :: String -> EvalState -> IO EvalState
processInput input state =
    case parseTokens $ tokenizer input of       -- handle parsing
        Left err -> (putStrLn $ prettyErr err) >> pure state
        Right program -> do
            result <- evalProgram program state -- handle evaluation of program
            case result of
                Left err -> (putStrLn $ prettyErr err) >> pure state
                Right newState -> do           
                    printLastVal newState state  -- print top value stack
                    pure newState                -- return new state


-- Helper functions to print out stack and dictionary

printStack :: EvalState -> IO ()
printStack (stk,_) = putStrLn $ "Stack: " ++ show stk

printLastVal :: EvalState -> EvalState -> IO () 
printLastVal (x:_,_) (oldStk,_) = 
    if (x:oldStk) == oldStk            -- check if old state is the same
        then putStr ""                 -- newline
        else putStrLn $ prettyValue x
printLastVal ([],_) _ = putStr ""      -- newline

printDictionary :: EvalState -> IO ()
printDictionary (_,eval) = putStrLn $ "Map: " ++ show eval



