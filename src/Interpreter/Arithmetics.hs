module Interpreter.Arithmetics(
    arithmeticsOps,
    evalArithmetics
) where

-- Internal libs
import Bprog.Types
import Bprog.Errors
import Interpreter.StackOp

-- List of allowed arithmetic operations
arithmeticsOps :: [String]
arithmeticsOps = ["+", "-", "*","/","div","<",">","==","&&","||", "not"]

-- evalArithmetics
--
-- Will patternmatch after allowed ruleset
evalArithmetics :: String -> EvalState -> IO (Either BprogError EvalState)
evalArithmetics op (stk,dict) = case (op,stk) of
    -- Integer arithmetics
    ("+", Numbo n2 : Numbo n1 : rest) -> push (Numbo (n1+n2)) (rest,dict)
    ("+", Numbo n2 : Deci f1 : rest) -> push (Deci (f1+fromIntegral n2)) (rest,dict) 
    ("+", Deci f2 : Numbo n1 : rest) -> push (Deci (fromIntegral n1 + f2)) (rest,dict) 
    ("+", Deci f2 : Deci f1 : rest) -> push (Deci (f1+f2)) (rest,dict) 

    ("-", Numbo n2 : Numbo n1 : rest) -> push (Numbo (n1-n2)) (rest,dict) 
    ("-", Numbo n2 : Deci f1 : rest) -> push (Deci (f1-fromIntegral n2)) (rest,dict) 
    ("-", Deci f2 : Numbo n1 : rest) -> push (Deci (fromIntegral n1 - f2)) (rest,dict) 
    ("-", Deci f2 : Deci f1 : rest) -> push (Deci (f1-f2)) (rest,dict)

    ("*", Numbo n2 : Numbo n1 : rest) -> push (Numbo (n1*n2)) (rest,dict) 
    ("*", Numbo n2 : Deci f1 : rest) -> push (Deci (f1*fromIntegral n2)) (rest,dict) 
    ("*", Deci f2 : Numbo n1 : rest) -> push (Deci (fromIntegral n1*f2)) (rest,dict) 
    ("*", Deci f2 : Deci f1 : rest) -> push (Deci (f1*f2)) (rest,dict) 

    -- Integer division
    ("div", Numbo 0 : _ : _) -> pure $ Left (RunTime DivisionByZero)
    ("div", Numbo n2 : Numbo n1 : rest) -> push (Numbo (div n1 n2)) (rest,dict) 

    ("div", Deci 0.0 : _ : _) -> pure $ Left (RunTime DivisionByZero)
    ("div", Deci f2 : Numbo n1 : rest) -> push (Numbo (div n1 (truncate f2))) (rest,dict)
    
    ("div", Numbo 0 : _ : _) -> pure $ Left (RunTime DivisionByZero)
    ("div", Numbo n2 : Deci f1 : rest) -> push (Numbo (div (truncate f1) n2)) (rest,dict)

    ("div", Deci 0.0 : _ : _) -> pure $ Left (RunTime DivisionByZero)
    ("div", Deci f2 : Deci f1 : rest) -> 
        push (Numbo (div (truncate f1) (truncate f2))) (rest,dict)
    
    -- Float division
    ("/", Numbo 0 : Numbo _ : _) -> pure $ Left (RunTime DivisionByZero)
    ("/", Numbo n2 : Numbo n1 : rest) -> 
        push (Deci (fromIntegral n1 / fromIntegral n2)) (rest,dict) 

    ("/", Numbo 0 : Deci _ : _) -> pure $ Left (RunTime DivisionByZero) 
    ("/", Numbo n2 : Deci f1 : rest) -> push (Deci ( f1 / fromIntegral n2)) (rest,dict) 

    ("/", Deci 0.0 : Numbo _ : _) -> pure $ Left (RunTime DivisionByZero) 
    ("/", Deci f2 : Numbo n1 : rest) -> push (Deci (fromIntegral n1 / f2)) (rest,dict) 

    ("/", Deci 0.0 : Deci _ : _) -> pure $ Left (RunTime DivisionByZero)  
    ("/", Deci f2 : Deci f1 : rest) -> push (Deci (f1 / f2)) (rest,dict) 

    
    ("<", Numbo n2 : Numbo n1 : rest) -> push (Truthy (n1 < n2)) (rest,dict)
    ("<", Deci f2 : Numbo n1 : rest) -> push (Truthy (fromIntegral n1 < f2)) (rest,dict)
    ("<", Numbo n2 : Deci f1 : rest) -> push (Truthy (f1 < fromIntegral n2)) (rest,dict)
    ("<", Deci f2 : Deci f1 : rest) -> push (Truthy (f1 < f2)) (rest,dict)
    
    (">", Numbo n2 : Numbo n1 : rest) -> push (Truthy (n1 > n2)) (rest,dict)
    (">", Deci f2 : Numbo n1 : rest) -> push (Truthy (fromIntegral n1 > f2)) (rest,dict)
    (">", Numbo n2 : Deci f1 : rest) -> push (Truthy (f1 > fromIntegral n2)) (rest,dict)
    (">", Deci f2 : Deci f1 : rest) -> push (Truthy (f1 > f2)) (rest,dict)

    (">", _ : _ : _) -> pure $ Left (RunTime ExpectedBoolOrNumber) 
    ("<", _ : _ : _) -> pure $ Left (RunTime ExpectedBoolOrNumber) 


    ("==", Deci f2 : Numbo n1 : rest) -> push (Truthy (fromIntegral n1 == f2)) (rest,dict)
    ("==", Numbo n2 : Deci f1 : rest) -> push (Truthy (f1 == fromIntegral n2)) (rest,dict)
    ("==", b : a : rest) -> push (Truthy (a == b)) (rest,dict)

    ("&&", Truthy b2 : Truthy b1 : rest) -> push (Truthy (b1 && b2)) (rest,dict)
    ("||", Truthy b2 : Truthy b1 : rest) -> push (Truthy (b1 || b2)) (rest,dict)

    ("not", Truthy b : rest) -> push (Truthy (if b == True then False else True)) (rest,dict)
    ("not", Numbo n : rest) -> push (Numbo (0-n)) (rest,dict)

    _ -> pure $ Left (RunTime ExpectedBoolOrNumber)
