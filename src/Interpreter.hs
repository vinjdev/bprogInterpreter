module Interpreter (
    evalProgram
) where


-- | Interpreter
-- Runtime evaluations
import Types
import Errors
import qualified Data.Map as Map
import Arithmetics
import ListOp
import StackOp
import BprogIO


-- Evaluates parsed input with program logic
--
-- Eager evaluation, so it will try to evaluate as it runs through 
--
-- Returns IO, Error or State (Stack,Dictionary)
--
-- TODO: LOAD FUNCTIONS FROM A MAP - USING LOOKUP
evalProgram :: [Types] -> EvalState -> IO (Either BprogError EvalState)
evalProgram [] state = pure $ Right state
evalProgram (x:xs) state = do
    result <- eval x state
    case result of
        Left err -> pure $ Left err
        Right newState -> evalProgram xs newState

-- Evaluate the input, and add them to the stack or dictionary
eval :: Types -> EvalState -> IO (Either BprogError EvalState)
eval val (stk,env) = case val of 

    -- Pushing data types onto the stack
    Numbo n -> push (Numbo n) (stk,env) 
    Deci f -> push (Deci f) (stk,env) 
    Truthy b -> push (Truthy b) (stk,env) 
    Wordsy s -> push (Wordsy s) (stk,env) 
    Bag xs -> push (Bag xs) (stk,env) 
    --Block xs -> push (Block xs) (stk,env)

    Block xs -> 
        case stk of
            --Tag "each" : Bag list : rest -> 
            --Tag "map" : Bag list : rest -> 
            --Tag "foldl" : Bag list : rest -> 
            --Block thenBlock : Tag "if" : rest ->
            --Block break : Tag "loop" : rest ->
            --Tag "times" : rest ->
            _ -> push (Block xs) (stk,env)
    
    Tag op
        | elem op arithmeticsOps -> evalArithmetics op (stk,env)
        | elem op listOps -> evalListOp op (stk,env)

    -- Stack operations
    Tag "dup" -> dup (stk,env)
    Tag "swap" -> swap (stk,env)
    Tag "pop" -> pop (stk,env)

    -- IO Operations
    Tag "print" -> printOp (stk,env)
    Tag "read" -> readOp (stk,env)

    -- Function and variable assignment
    Tag ":=" -> 
        case stk of
            value : Tag name : rest -> pure $ Right (rest, Map.insert name value env)
            _                     -> pure $ Left (RunTime ExpectedVariable)

    Tag "fun" -> 
        case stk of
            value : Tag name : rest -> pure $ Right (rest, Map.insert name value env)
            _                     -> pure $ Left (RunTime ExpectedVariable)

    -- Executes a code block, which is at the top of the stack
    Tag "exec" ->
        case stk of
            Block code : rest -> evalProgram code (rest,env)
            _ -> pure $ Left (RunTime ExpectedVariable)

    -- Function call
    Tag sym ->
        case Map.lookup sym env of
            Just (Block body) -> evalProgram body (stk,env) -- Evaluate function body
            Just value -> pure $ Right (value : stk, env)   -- Evaluate a value
            Nothing  -> pure $ Right (Tag sym : stk,env)

