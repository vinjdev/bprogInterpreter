module Arithmetics(
    arithmeticsOps,
    evalArithmetics
) where

import Types
import Errors

arithmeticsOps :: [String]
arithmeticsOps = ["+", "-", "*","/"]

evalArithmetics :: String -> EvalState -> IO(Either BprogError EvalState)
evalArithmetics op (stk,env) = case (op,stk) of
    ("+", Numbo n : Numbo n2 : rest) -> pure $ Right (Numbo (n+n2) : rest,env) 
    ("-", Numbo n : Numbo n2 : rest) -> pure $ Right (Numbo (n-n2) : rest,env) 
    ("*", Numbo n : Numbo n2 : rest) -> pure $ Right (Numbo (n*n2) : rest,env) 
    ("/", Numbo 0 : _ : _) -> pure $ Left (RunTime DivisionByZero)
    ("/", Numbo n : Numbo n2 : rest) -> pure $ Right (Numbo (div n2 n) : rest,env) 
    _ -> pure $ Left (RunTime ExpectedBoolOrNumber)
