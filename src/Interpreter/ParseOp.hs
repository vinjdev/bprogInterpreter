module Interpreter.ParseOp (
    parseOps,
    evalParse
) where

-- Internal libs
import Bprog.Types
import Bprog.Errors
import Interpreter.StackOp
import Interpreter.Dictionary

-- External libs
import Text.Read (readMaybe)

-- Allowed parse operations
parseOps :: [String]
parseOps = ["parseInteger","parseFloat","words"]

-- parseInteger
--
-- Reads a integer from a string
evalParse :: String -> EvalState -> IO(Either BprogError EvalState)
evalParse op (x : rest,dict) = do
    x' <- handleTags x dict
    case (op,x') of
        ("parseInteger",Wordsy str) -> do
            case readMaybe str :: Maybe Integer of
                Just n  -> push (Numbo n) (rest,dict)
                Nothing -> pure $ Left (RunTime ExpectedString) 

        ("parseFloat", Wordsy str) -> do
            case readMaybe str :: Maybe Float of
                Just f  -> push (Deci f) (rest,dict)
                Nothing -> pure $ Left (RunTime ExpectedString)

        ("words", Wordsy str) -> do
            let parts  = words str
                result = Bag (map Wordsy parts)
             in push result (rest,dict)

        _ -> pure $ Left (RunTime ExpectedString) 

evalParse _ _ = pure $ Left (RunTime ExpectedString)
