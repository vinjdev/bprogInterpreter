module Bprog.NormalMode (
    runNormal
) where

-- Internal libs
import Interpreter.Interpreter
import Bprog.Parser
import Bprog.Errors

-- External libs
import qualified Data.Map as Map

-- Runs a bprog file, based on a file path
runNormal :: FilePath -> IO ()
runNormal path = do
    putStrLn $ "Reading file from: " ++ show path
    content <- readFile path
    case parseTokens $ tokenizer content of
        Left err -> (putStrLn $ prettyErr err)
        Right program -> do
            result <- evalProgram program ([],Map.empty)
            case result of
                Left err -> (putStrLn $ prettyErr err)
                Right (stk,_) -> putStrLn $ "Final Stack: " ++ show stk
