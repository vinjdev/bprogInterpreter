module Interpreter.Arithmetics(
    arithmeticsOps,
    evalArithmetics
) where

-- Internal libs
import Bprog.Types
import Bprog.Errors
import Interpreter.StackOp
import Interpreter.Dictionary 

-- List of allowed arithmetic operations
arithmeticsOps :: [String]
arithmeticsOps = ["+", "-", "*","/","div","<",">","==","&&","||", "not"]

-- evalArithmetics
--
-- Will patternmatch after allowed ruleset
evalArithmetics :: String -> EvalState -> IO (Either BprogError EvalState)

-- Special case for: not
-- only needs one value on the stack
evalArithmetics "not" (x:rest,dict) = do
    x' <- handleTags x dict
    case x' of
        Truthy b -> push (Truthy (not b)) (rest,dict)
        Numbo n  -> push (Numbo (0-n)) (rest,dict)
        _        -> pure $ Left (RunTime ExpectedBoolOrNumber) 

-- Runtime error when the stack is empty
evalArithmetics _ ([], _) = pure $ Left (RunTime StackEmpty)
evalArithmetics _ ([_], _) = pure $ Left (RunTime StackEmpty)

evalArithmetics op (x:y:rest,dict) = do
    x' <- handleTags x dict
    y' <- handleTags y dict
    case (op,x',y') of
        -- Integer arithmetics
        ("+", Numbo n2, Numbo n1) -> push (Numbo (n1+n2)) (rest,dict)
        ("+", Numbo n2, Deci f1) -> push (Deci (f1+fromIntegral n2)) (rest,dict) 
        ("+", Deci f2, Numbo n1) -> push (Deci (fromIntegral n1 + f2)) (rest,dict) 
        ("+", Deci f2, Deci f1) -> push (Deci (f1+f2)) (rest,dict) 

        ("-", Numbo n2, Numbo n1) -> push (Numbo (n1-n2)) (rest,dict) 
        ("-", Numbo n2, Deci f1) -> push (Deci (f1-fromIntegral n2)) (rest,dict) 
        ("-", Deci f2, Numbo n1) -> push (Deci (fromIntegral n1 - f2)) (rest,dict) 
        ("-", Deci f2, Deci f1) -> push (Deci (f1-f2)) (rest,dict)

        ("*", Numbo n2, Numbo n1) -> push (Numbo (n1*n2)) (rest,dict) 
        ("*", Numbo n2, Deci f1) -> push (Deci (f1*fromIntegral n2)) (rest,dict) 
        ("*", Deci f2, Numbo n1) -> push (Deci (fromIntegral n1*f2)) (rest,dict) 
        ("*", Deci f2, Deci f1) -> push (Deci (f1*f2)) (rest,dict) 

        -- Integer division
        ("div", Numbo 0, Numbo _) -> pure $ Left (RunTime DivisionByZero)
        ("div", Deci 0.0, Numbo _) -> pure $ Left (RunTime DivisionByZero)
        ("div", Numbo 0, Deci _) -> pure $ Left (RunTime DivisionByZero)
        ("div", Deci 0.0, Deci _) -> pure $ Left (RunTime DivisionByZero)

        ("div", Numbo n2, Numbo n1) -> push (Numbo (div n1 n2)) (rest,dict) 
        ("div", Deci f2, Numbo n1) -> push (Numbo (div n1 (truncate f2))) (rest,dict)
        ("div", Numbo n2, Deci f1) -> push (Numbo (div (truncate f1) n2)) (rest,dict)
        ("div", Deci f2, Deci f1) -> 
            push (Numbo (div (truncate f1) (truncate f2))) (rest,dict)
        
        -- Float division
        ("/", Numbo 0, Numbo _) -> pure $ Left (RunTime DivisionByZero)
        ("/", Numbo 0, Deci _) -> pure $ Left (RunTime DivisionByZero) 
        ("/", Deci 0.0, Numbo _) -> pure $ Left (RunTime DivisionByZero) 
        ("/", Deci 0.0, Deci _) -> pure $ Left (RunTime DivisionByZero)  

        ("/", Numbo n2, Numbo n1) -> 
            push (Deci (fromIntegral n1 / fromIntegral n2)) (rest,dict) 
        ("/", Numbo n2, Deci f1) -> push (Deci ( f1 / fromIntegral n2)) (rest,dict) 
        ("/", Deci f2, Numbo n1) -> push (Deci (fromIntegral n1 / f2)) (rest,dict) 
        ("/", Deci f2, Deci f1) -> push (Deci (f1 / f2)) (rest,dict) 

        -- Boolean Expressions
        ("<", Numbo n2, Numbo n1) -> push (Truthy (n1 < n2)) (rest,dict)
        ("<", Deci f2, Numbo n1) -> push (Truthy (fromIntegral n1 < f2)) (rest,dict)
        ("<", Numbo n2, Deci f1) -> push (Truthy (f1 < fromIntegral n2)) (rest,dict)
        ("<", Deci f2, Deci f1) -> push (Truthy (f1 < f2)) (rest,dict)
        
        (">", Numbo n2, Numbo n1) -> push (Truthy (n1 > n2)) (rest,dict)
        (">", Deci f2, Numbo n1) -> push (Truthy (fromIntegral n1 > f2)) (rest,dict)
        (">", Numbo n2, Deci f1) -> push (Truthy (f1 > fromIntegral n2)) (rest,dict)
        (">", Deci f2, Deci f1) -> push (Truthy (f1 > f2)) (rest,dict)

        (">", _ , _) -> pure $ Left (RunTime ExpectedBoolOrNumber) 
        ("<", _, _) -> pure $ Left (RunTime ExpectedBoolOrNumber) 


        ("==", Deci f2, Numbo n1) -> push (Truthy (fromIntegral n1 == f2)) (rest,dict)
        ("==", Numbo n2, Deci f1) -> push (Truthy (f1 == fromIntegral n2)) (rest,dict)
        ("==", b, a) -> push (Truthy (a == b)) (rest,dict)

        ("&&", Truthy b2, Truthy b1) -> push (Truthy (b1 && b2)) (rest,dict)
        ("||", Truthy b2, Truthy b1) -> push (Truthy (b1 || b2)) (rest,dict)


        -- fallback error
        _ -> pure $ Left (RunTime ExpectedBoolOrNumber)
