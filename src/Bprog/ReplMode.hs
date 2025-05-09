module Bprog.ReplMode (
    startRepl
) where

-- Internal libs
import Bprog.Types
import Bprog.Errors
import Bprog.Parser
import Interpreter.Interpreter
import Interpreter.BprogIO

-- External libs
import System.IO (hFlush, stdout)
import qualified Data.Map as Map

-- Init the repl mode, with a empty stack and dictionary
startRepl :: IO ()
startRepl = do
    putStrLn "Welcome to the BPROG REPL"
    replLoop ([],Map.empty) 

-- repl loop
replLoop :: EvalState -> IO ()
replLoop state@(stk,_) = do
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
        -- Importing prelude library
        "prelude" -> do
            putStrLn "Importing prelude library..."
            content <- readFile "src/prelude/prelude.bprog"
            interpretInput content state >>= replLoop
        -- Flushing dictionary
        ":D" -> replLoop (stk,Map.empty)
        -- Make a new State
        _ -> interpretInput input state >>= replLoop 

-- parses and interpret the program
interpretInput :: String -> EvalState -> IO EvalState
interpretInput input state =
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

-- Prints the whole stack
printStack :: EvalState -> IO ()
printStack (stk,_) = putStrLn $ "Stack: " ++ show stk

-- Prints the top value of the stack
printLastVal :: EvalState -> EvalState -> IO () 
printLastVal (x:_,_) (oldStk,_) = 
    if (x:oldStk) == oldStk            -- check if old state is the same
        then putStr ""                 -- newline
        else putStrLn $ prettyValue x
printLastVal ([],_) _ = putStr ""      -- newline

-- Prints the whole dictionary
printDictionary :: EvalState -> IO ()
printDictionary (_,eval) = putStrLn $ "Map: " ++ show eval



