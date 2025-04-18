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
import Control.Monad (foldM)


-- | EvalProgram
-- Evaluates parsed input with program logic
-- Eager evaluation, so it will try to evaluate as it runs through 
-- Returns IO, Error or State (Stack,Dictionary)
-- TODO: LOAD FUNCTIONS FROM A MAP - USING LOOKUP
evalProgram :: [Types] -> EvalState -> IO (Either BprogError EvalState)
evalProgram [] state = pure $ Right state

-- Look ahead function for "swap"
evalProgram (val1 : val2 : Tag "swap" : rest) (stk,dict) =
    evalProgram rest (val1:val2:stk,dict)

-- Look ahead function for function assignment ":="
evalProgram (Tag name : value : Tag ":=" : rest) (stk,dict) =
    evalProgram  rest (stk,Map.insert name value dict)

-- Look ahead function for function assignment "fun"
evalProgram (Tag name : value : Tag "fun" : rest) (stk,dict) = 
    evalProgram rest (stk,Map.insert name value dict)

evalProgram (x:xs) state = do
    result <- eval x state
    case result of
        Left err -> pure $ Left err
        Right newState -> evalProgram xs newState

eval :: Types -> EvalState -> IO (Either BprogError EvalState)
eval val state@(stk,env) = case val of

    -- Pushing data types onto the stack
    Numbo n -> push (Numbo n) state
    Deci f -> push (Deci f) state
    Truthy b -> push (Truthy b) state 
    Wordsy s -> push (Wordsy s) state 
    Bag xs -> push (Bag xs) state
    
    -- arithmetics and list operations
    Tag op
        | elem op arithmeticsOps -> evalArithmetics op state
        | elem op listOps -> evalListOp op state

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
            Tag "each" : Bag list : rest -> do
                let runEach acc el = do
                        case acc of
                            Left err -> pure $ Left err
                            Right (s,d) -> evalProgram code (el : s,d)

                result <- foldM runEach (Right (rest,env)) list 
                pure result   
            Tag "each" : _ : _ -> pure $ Left (RunTime ExpectedList)
                
            Tag "map" : Bag list : rest -> do
                let evalOne el = evalProgram code (el : rest,env) -- el: element in the list

                result <- mapM evalOne list -- result: [Right ([2],env)]
                case sequence result of     -- result: Right [([2],env)]
                    Left err -> pure $ Left err
                    Right states -> case traverse extractTop states of  
                                        Nothing -> pure $ Left (RunTime ExpectedQuotation)
                                        Just newValues -> pure $ Right (Bag newValues : rest,env)
                where
                    extractTop (x:_,_) = Just x
                    extractTop _ = Nothing
            Tag "map" : _ : _ -> pure $ Left (RunTime ExpectedList)
            
            --Tag "foldl" : Numbo n : Bag list : rest ->
                
            Block thenBlock : Tag "if" : Truthy b : rest ->
                if Truthy b == Truthy True then evalProgram thenBlock (rest,env) -- true block
                                           else evalProgram code (rest,env)      -- false block

            Block _ : Tag "if" : _ : _ -> pure $ Left (RunTime ExpectedBool)

            -- Loop
            Block breakCond : Tag "loop" : rest -> do
                -- evaluate the break code to be boolean operation
                let runLoop (s,d) = do
                        condResult <- evalProgram breakCond (s,d)
                        case condResult of
                            Left err -> pure $ Left err
                            Right (s',d') -> case s' of
                                (Truthy True : s'') -> do
                                    bodyResult <- evalProgram code (s'',d')
                                    case bodyResult of
                                        Left err -> pure $ Left err
                                        Right newState -> runLoop newState
                                (Truthy False : s'') -> pure $ Right (s'',d')
                                _ -> pure $ Left (RunTime ExpectedBool)
                runLoop (rest,env)                 

            -- Times
            Tag "times" : Numbo n : rest -> do
                let runNTimes 0 state' = pure $ Right state'
                    runNTimes i state' = do
                        result <- evalProgram code state'
                        case result of
                            Left err -> pure $ Left err
                            Right newState -> runNTimes (i-1) newState
                runNTimes n (rest,env) 
            Tag "times" : _ : _ -> pure $ Left (RunTime ExpectedBoolOrNumber)
                

            -- pushing code block to stack, if no operations
            _ -> push (Block code) state
