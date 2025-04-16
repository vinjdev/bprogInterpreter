module NormalMode (
    runNormal
) where

import Interpreter
import Parser
import Errors
import qualified Data.Map as Map


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
