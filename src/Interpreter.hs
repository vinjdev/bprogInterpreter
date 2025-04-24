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
import ParseOp
import Control.Monad (foldM)


-- | EvalProgram
-- Evaluates parsed input with program logic
-- Eager evaluation, so it will try to evaluate as it runs through 
-- Returns IO, Error or State (Stack,Dictionary)

-- Error Handler
evalProgram :: [Types] -> EvalState -> IO (Either BprogError EvalState)
evalProgram [] state = pure $ Right state

-- Evaluation loop
evalProgram (x:xs) state = do
    result <- eval x state
    case result of
        Left err -> pure $ Left err
        Right newState -> evalProgram xs newState

-- Handles cases of evaluation
eval :: Types -> EvalState -> IO (Either BprogError EvalState)
eval val state@(stk,env) = case val of

    -- Pushing data types onto the stack
    Numbo n -> push (Numbo n) state
    Deci f -> push (Deci f) state
    Truthy b -> push (Truthy b) state 
    Wordsy s -> push (Wordsy s) state 
    -- Handle evaluations within the list
    Bag xs -> do
        result <- mapM (\x -> evalProgram [x] state) xs
        case sequence result of
            Left err -> pure $ Left err
            Right newState -> case traverse extractTop newState of
                Nothing -> pure $ Left (RunTime ExpectedVariable)
                Just evaluated -> push (Bag evaluated) state 

        where
            extractTop (x:_,_) = Just x
            extractTop _ = Nothing
    
    -- arithmetics, list and parser operations
    Tag op
        | elem op arithmeticsOps -> evalArithmetics op state
        | elem op listOps -> evalListOp op state
        | elem op parseOps -> evalParse op state

    -- Stack operations
    Tag "dup" -> dup state
    Tag "swap" -> swap state 
    Tag "pop" -> pop state 

    -- IO Operations
    Tag "print" -> printOp state 
    Tag "read" -> readOp state
    
    -- Function and variable assignment
    Tag ":=" -> 
        case stk of
            value : Tag name : rest -> pure $ Right (rest, Map.insert name value env)
            _                       -> pure $ Left (RunTime ExpectedVariable)

    Tag "fun" -> 
        case stk of
            value : Tag name : rest -> pure $ Right (rest, Map.insert name value env)
            _                     -> pure $ Left (RunTime ExpectedVariable)

    -- Executes a code block, which is at the top of the stack
    Tag "exec" ->
        case stk of
            Block code : rest -> evalProgram code (rest,env)
            _ -> pure $ Left (RunTime ExpectedQuotation)

    -- Function call, or push 
    Tag sym -> 
        case Map.lookup sym env of
            Just (Block body) -> evalProgram body state     
            Just value -> push value state                  
            Nothing  -> push (Tag sym) state               

    -- Code Block operations
    Block code -> 
        case stk of
            Tag "each" : Bag list : rest -> evalEachBlock code list (rest,env)
            Tag "each" : _ : _ -> pure $ Left (RunTime ExpectedList)
                
            Tag "map" : Bag list : rest -> evalMapBlock code list (rest,env)
            Tag "map" : _ : _ -> pure $ Left (RunTime ExpectedList)
            
            --Tag "foldl" : Numbo n : Bag list : rest ->
                
            Block thenBlock : Tag "if" : Truthy b : rest ->
                if Truthy b == Truthy True then evalProgram thenBlock (rest,env) -- true block
                                           else evalProgram code (rest,env)      -- false block

            Block _ : Tag "if" : _ : _ -> pure $ Left (RunTime ExpectedBool)

            -- Loop: BREAKS ON TRUE
            Block breakCond : Tag "loop" : rest -> evalLoopBlock breakCond code (rest,env) 

            -- Times
            Tag "times" : Numbo n : rest -> evalTimesBlock code n (rest,env)
            Tag "times" : _ : _ -> pure $ Left (RunTime ExpectedBoolOrNumber)
                

            -- pushing code block to stack, if no operations
            _ -> push (Block code) state

evalEachBlock :: [Types] -> [Types] -> EvalState -> IO(Either BprogError EvalState)
evalEachBlock code list (rest,dict) = do
        let runEach acc el = do
                case acc of
                    Left err -> pure $ Left err
                    Right (s,d) -> evalProgram code (el : s,d)
        foldM runEach (Right (rest,dict)) list 

evalMapBlock :: [Types] -> [Types] -> EvalState -> IO (Either BprogError EvalState)
evalMapBlock code list (rest,dict) = do
                let evalOne el = evalProgram code (el : rest,dict) -- el: element in the list

                result <- mapM evalOne list -- result: [Right ([2],env)]
                case sequence result of     -- result: Right [([2],env)]
                    Left err -> pure $ Left err
                    Right states -> case traverse extractTop states of  
                                        Nothing -> pure $ Left (RunTime ExpectedQuotation)
                                        Just newValues -> pure $ Right (Bag newValues : rest,dict)
                where
                    extractTop (x:_,_) = Just x
                    extractTop _ = Nothing

evalLoopBlock :: [Types] -> [Types] -> EvalState -> IO (Either BprogError EvalState)
evalLoopBlock breakCond code (rest,dict) = do
                -- evaluate the break code to be boolean operation
                let runLoop (s,d) = do
                        condResult <- evalProgram breakCond (s,d)
                        case condResult of
                            Left err -> pure $ Left err
                            Right (s',d') -> case s' of
                                (Truthy True : s'') -> pure $ Right (s'',d')
                                (Truthy False : s'') -> do
                                    bodyResult <- evalProgram code (s'',d')
                                    case bodyResult of
                                        Left err -> pure $ Left err
                                        Right newState -> runLoop newState                                
                                _ -> pure $ Left (RunTime ExpectedBool)
                runLoop (rest,dict)

evalTimesBlock :: [Types] -> Integer -> EvalState -> IO (Either BprogError EvalState) 
evalTimesBlock code count (rest,dict) = do
                let runNTimes 0 state' = pure $ Right state'
                    runNTimes i state' = do
                        result <- evalProgram code state'
                        case result of
                            Left err -> pure $ Left err
                            Right newState -> runNTimes (i-1) newState
                runNTimes count (rest,dict)
