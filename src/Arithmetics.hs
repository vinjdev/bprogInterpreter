module Arithmetics(
    arithmeticsOps,
    evalArithmetics
) where

import Types
import Errors

arithmeticsOps :: [String]
arithmeticsOps = ["+", "-", "*","/"]

evalArithmetics :: String -> EvalState -> Either BprogError EvalState
evalArithmetics op (stk,env) = case (op,stk) of
    ("+", Numbo n : Numbo n2 : rest) -> Right (Numbo (n+n2) : rest,env) 
    ("-", Numbo n : Numbo n2 : rest) -> Right (Numbo (n-n2) : rest,env) 
    ("*", Numbo n : Numbo n2 : rest) -> Right (Numbo (n*n2) : rest,env) 
    ("/", Numbo 0 : _ : _) -> Left (RunTime DivisionByZero)
    ("/", Numbo n : Numbo n2 : rest) -> Right (Numbo (div n2 n) : rest,env) 
    _ -> Left (RunTime ExpectedBoolOrNumber)
