module ParseOp (
    parseOps,
    evalParse
) where

import Types
import Errors
import Text.Read (readMaybe)

parseOps :: [String]
parseOps = ["parseInteger","parseFloat","words"]

evalParse :: String -> EvalState -> IO(Either BprogError EvalState)
evalParse "parseInteger" (Wordsy str : rest,dict) = 
    case readMaybe str :: Maybe Integer of
        Just n  -> pure $ Right (Numbo n : rest, dict)
        Nothing -> pure $ Left (RunTime ExpectedInteger)

evalParse "parseFloat" (Wordsy str : rest,dict) = 
    case readMaybe str :: Maybe Float of
        Just f  -> pure $ Right (Deci f : rest, dict)
        Nothing -> pure $ Left (RunTime ExpectedInteger)

evalParse "words" (Wordsy str : rest, dict) = 
    let parts = words str
        result = Bag (map Wordsy parts)
    in pure $ Right (result : rest,dict)

evalParse _ _ = pure $ Left (RunTime ExpectedString)
