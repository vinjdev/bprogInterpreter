module Interpreter.ParseOp (
    parseOps,
    evalParse
) where

-- Internal libs
import Bprog.Types
import Bprog.Errors
import Interpreter.StackOp

-- External libs
import Text.Read (readMaybe)

-- Allowed parse operations
parseOps :: [String]
parseOps = ["parseInteger","parseFloat","words"]

-- parseInteger
--
-- Reads a integer from a string
evalParse :: String -> EvalState -> IO(Either BprogError EvalState)
evalParse "parseInteger" (Wordsy str : rest,dict) = 
    case readMaybe str :: Maybe Integer of
        Just n  -> push (Numbo n) (rest, dict)
        Nothing -> pure $ Left (RunTime ExpectedInteger)

-- parseFloat
--
-- Reads a float from a string
evalParse "parseFloat" (Wordsy str : rest,dict) = 
    case readMaybe str :: Maybe Float of
        Just f  -> push (Deci f) (rest, dict)
        Nothing -> pure $ Left (RunTime ExpectedInteger)

-- words
--
-- Takes a string, and turns into a list
evalParse "words" (Wordsy str : rest, dict) = 
    let parts = words str
        result = Bag (map Wordsy parts)
    in push (result) (rest,dict)

evalParse _ _ = pure $ Left (RunTime ExpectedString)
