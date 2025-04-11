module Interpreter (
    evalProgram
) where


-- | Interpreter
-- Runtime evaluations

import Types
import Errors
import MyMap
import BprogMonad

type EvalState = (Stack, Dictionary)

-- Evalualte the parsed program 
evalProgram :: [Types] -> Bprog ()
evalProgram [] state = Right state
evalProgram (x:xs) = do
    eval x
    evalProgram xs


eval :: Types -> Bprog ()
eval val = case val of 
    
    -- Standard literals
    Numbo n -> modifyStack (Numbo n :)
    Deci f -> modifyStack (Deci f :)
    Truthy b -> modifyStack (Truthy b :)
    Wordsy w -> modifyStack (Wordsy w :) 
    Bag xs -> modifyStack (Bag xs :) 
    Block xs -> modifyStack (Block xs:)
        
    -- Stack Operations
    Tag "dup" -> do
        stk <- getStack
        case stk of
            (x:xs) -> putStack (x:x:xs)
            []     -> throwB $ RunTime StackEmpty
    
    Tag "swap" -> do
        stk <- getStack
        case stk of
            (x:y:xs) -> putStack (y:x:xs)
            _        -> throwB $ RunTime StackEmpty

    Tag "pop" -> do
        stk <- getStack
        case stk of
            (_:xs) -> putStack (xs)
            []     -> throwB $ RunTime StackEmpty
    
    -- Assignments
    Tag ":=" -> do
        stk <- getStack
        case stk of
            val : Tag name : rest -> do
                dict <- getDict
                putDict (myInsert name val dict)
                putStack rest
            _ -> throwB $ RunTime ExpectedVariable
            

    -- Tag lookup
    Tag sym -> do
        dict <- getDict
        case myLookup sym dict of
            Just val2 -> eval val2
            Nothing -> modifyStack (Tag sym :)
        

    
    



