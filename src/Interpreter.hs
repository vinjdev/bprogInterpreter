module Interpreter (
    evalProgram
) where


-- | Interpreter
-- Runtime evaluations
import Types
import Errors
import qualified Data.Map as Map
import Arithmetics
import System.IO (hFlush, stdout)
import ListOp

push :: Types -> EvalState -> EvalState
push val (stk,env) = (val : stk,env)


-- Evaluates parsed input with program logic
--
-- Eager evaluation, so it will try to evaluate as it runs through 
--
-- Returns IO, Error or State (Stack,Dictionary)
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
    Numbo n -> pure $ Right $ push (Numbo n) (stk,env)
    Deci f -> pure $ Right $ push (Deci f) (stk,env)
    Truthy b -> pure $ Right $ push (Truthy b) (stk,env)
    Wordsy w -> pure $ Right $ push (Wordsy w) (stk,env)
    Bag xs -> pure $ Right $ push (Bag xs) (stk,env)
    --Block xs -> pure $ Right $ push (Block xs) (stk,env)

    Block xs -> 
        case stk of
            --Tag "each" : Bag list : rest -> 
            --Tag "map" : Bag list : rest -> 
            --Tag "foldl" : Bag list : rest -> 
            --Block thenBlock : Tag "if" : rest ->
            --Block break : Tag "loop" : rest ->
            --Tag "times" : rest ->
            _ -> pure $ Right $ push (Block xs) (stk,env)

    -- IO Operations
    Tag "print" ->
        case stk of
            x:xs -> do
                print x
                pure $ Right (xs,env)
            [] -> pure $ Left (RunTime StackEmpty)

    Tag "read" -> do
        putStr "Input: "
        hFlush stdout
        input <- getLine
        pure $ Right (Wordsy input : stk,env)
            
    Tag op
        | elem op arithmeticsOps -> pure $ evalArithmetics op (stk,env)
        | elem op listOps -> pure $ evalListOp op (stk,env)

    -- Stack operations
    Tag "dup" ->
        case stk of
            (x:xs) -> pure $ Right (x:x:xs,env)
            []    -> pure $ Left (RunTime StackEmpty)

    Tag "swap" ->
        case stk of
            (x:y:xs) -> pure $ Right (y:x:xs,env)
            _       -> pure $ Left (RunTime StackEmpty)

    Tag "pop" ->
        case stk of
            (_:xs) -> pure $ Right (xs, env)
            []    -> pure $ Left (RunTime StackEmpty) 

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

