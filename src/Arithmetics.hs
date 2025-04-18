module Arithmetics(
    arithmeticsOps,
    evalArithmetics
) where

import Types
import Errors

arithmeticsOps :: [String]
arithmeticsOps = ["+", "-", "*","/","<",">"]

evalArithmetics :: String -> EvalState -> IO(Either BprogError EvalState)
evalArithmetics op (stk,dict) = case (op,stk) of
    ("+", Numbo n2 : Numbo n1 : rest) -> pure $ Right (Numbo (n1+n2) : rest,dict) 
    ("-", Numbo n2 : Numbo n1 : rest) -> pure $ Right (Numbo (n1-n2) : rest,dict) 
    ("*", Numbo n2 : Numbo n1 : rest) -> pure $ Right (Numbo (n1*n2) : rest,dict) 
    ("/", Numbo 0 : _ : _) -> pure $ Left (RunTime DivisionByZero)
    ("/", Numbo n : Numbo n2 : rest) -> pure $ Right (Numbo (div n2 n) : rest,dict) 

    ("<", Numbo n2 : Numbo n1 : rest) -> pure $ Right (Truthy (n1 < n2) : rest,dict)
    ("<", _ : _ : _) -> pure $ Left (RunTime ExpectedBoolOrNumber) 
    (">", Numbo n2 : Numbo n1 : rest) -> pure $ Right (Truthy (n1 > n2) : rest,dict)
    (">", _ : _ : _) -> pure $ Left (RunTime ExpectedBoolOrNumber) 

    _ -> pure $ Left (RunTime ExpectedBoolOrNumber)
