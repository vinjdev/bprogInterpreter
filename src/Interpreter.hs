module Interpreter (
    evalProgram
) where


-- | Interpreter
-- Runtime evaluations

import Types
import Errors
import qualified Data.Map as Map

type EvalState = (Stack, Dictionary)

push :: Types -> EvalState -> EvalState
push val (stk,env) = (val : stk,env)

eval :: Types -> EvalState -> Either BprogError EvalState
eval val (stk,env) = case val of 
    Numbo n -> Right $ push (Numbo n) (stk,env)
    Deci f -> Right $ push (Deci f) (stk,env)
    Truthy b -> Right $ push (Truthy b) (stk,env)
    Wordsy w -> Right $ push (Wordsy w) (stk,env)
    Bag xs -> Right $ push (Bag xs) (stk,env)
    Block xs -> Right $ push (Block xs) (stk,env)

    Tag ":=" -> 
        case stk of
            val : Tag name : rest -> Right (rest, Map.insert name val env)
            _                     -> Left (RunTime ExpectedVariable)

    Tag sym ->
        case Map.lookup sym env of
            Just val2 -> eval val2 (stk,env)
            Nothing   -> Right (Tag sym : stk,env)

    
    
    
evalProgram :: [Types] -> EvalState -> Either BprogError EvalState
evalProgram [] state = Right state
evalProgram (x:xs) state = do
    newState <- eval x state
    evalProgram xs newState


