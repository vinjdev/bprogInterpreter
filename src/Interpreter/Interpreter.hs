module Interpreter.Interpreter (
    evalProgram
) where


-- | Interpreter
-- Runtime evaluations

-- Internal libs
import Bprog.Types
import Bprog.Errors
import Interpreter.StackOp
import Interpreter.Arithmetics
import Interpreter.ListOp
import Interpreter.BprogIO
import Interpreter.ParseOp
import Interpreter.Dictionary

-- External libs
import qualified Data.Map as Map
import Control.Monad (foldM)

-- | EvalProgram
-- Evaluates parsed input with program logic
-- Eager evaluation, so it will try to evaluate as it runs through 
-- Returns IO, Error or State (Stack,Dictionary)

evalProgram :: [Types] -> EvalState -> IO (Either BprogError EvalState)


-- Returns state when there is no parsed input
evalProgram [] state = pure $ Right state

--  ================ Special lookahead functions =========================== 
-- List operations: each, map, foldl
-- Control flow: if, loop, times

-- each generic case
evalProgram (Tag "each" : code : rest ) (Bag list : stk,dict) = do
    result <- case code of
                Block codeB -> evalEachBlock codeB list (stk,dict)
                Bag _ -> pure $ Left (RunTime ExpectedQuotation)
                _ -> evalEachBlock [code] list (stk,dict)
    case result of
        Left err -> pure $ Left err
        Right newState -> evalProgram rest newState

evalProgram (Tag "each" : _ : _ ) ( _ : _,_) = 
    pure $ Left (RunTime ExpectedList)

-- Map
evalProgram (Tag "map" : code : rest ) (Bag list : stk, dict) = do
    result <- case code of
                Block codeB -> evalMapBlock codeB list (stk,dict)
                Bag _ -> pure $ Left (RunTime ExpectedQuotation)
                _ -> evalMapBlock [code] list (stk,dict)
    case result of
        Left err -> pure $ Left err
        Right newState -> evalProgram rest newState

-- map error
evalProgram (Tag "map" : _ : _ ) ( _ : _,_) = 
    pure $ Left (RunTime ExpectedList)
        

-- foldl generic case
evalProgram (Tag "foldl" : val : rest ) (n@(Numbo _) : Bag list : stk,dict) = do
    result <- case val of
            Block code -> evalFoldlBlock code n list (stk,dict)
            Bag _ -> pure $ Left (RunTime ExpectedQuotation)
            _ -> evalFoldlBlock [val] n list (stk,dict)
    case result of
        Left err -> pure $ Left err
        Right newState -> evalProgram rest newState

-- Foldl error
evalProgram (Tag "foldl" : _ : _ ) (_ : _ : _,_) =
    pure $ Left (RunTime ExpectedList)

-- Special case for if
evalProgram (Tag "if" : trueBlock : falseBlock : rest ) (Truthy b : stk,dict) = do
    let checkCode = if b then trueBlock else falseBlock 
    result <- case checkCode of
                Block code -> evalProgram code (stk,dict)
                _ -> eval checkCode (stk,dict)
    case result of
        Left err -> pure $ Left err
        Right newState -> evalProgram rest newState

-- if: error case: There is no boolean expression
evalProgram (Tag "if" : _ : _ : _ ) (_:_,_) =
    pure $ Left (RunTime ExpectedBool)

-- loop
evalProgram (Tag "loop" : Block cond : Block code : rest ) (stk,dict) = do
    result <- evalLoopBlock cond code (stk,dict)
    case result of
        Left err -> pure $ Left err
        Right newState -> evalProgram rest newState

evalProgram (Tag "loop" : _ : _ : _ ) (_,_) =
    pure $ Left (RunTime ExpectedQuotation)

-- times generic case
evalProgram (Tag "times" : code : rest ) (Numbo n : stk,dict) = do
    result <- case code of
                Block codeB -> evalTimesBlock codeB n (stk,dict)
                Bag _ -> pure $ Left (RunTime ExpectedQuotation)
                _ -> evalTimesBlock [code] n (stk,dict)
    case result of
        Left err -> pure $ Left err
        Right newState -> evalProgram rest newState 

evalProgram (Tag "times" : _ : _ ) ( _ : _,_) =
    pure $ Left (RunTime ExpectedInteger)



-- ===================== Evaluation loop =======================================

-- Main evaluation loop
evalProgram (x:xs) state = do
    result <- eval x state
    case result of
        Left err -> pure $ Left err
        Right newState -> evalProgram xs newState

-- Handle evaluation for one case
eval :: Types -> EvalState -> IO (Either BprogError EvalState)
eval val state@(_,dict) = case val of

    -- Pushing data types onto the stack
    Numbo n -> push (Numbo n) state
    Deci f -> push (Deci f) state
    Truthy b -> push (Truthy b) state 
    Wordsy s -> push (Wordsy s) state 

    -- Pushing a list onto the Stack
    Bag xs -> do 
        newList <- mapM (evalBag dict) xs
        push (Bag newList) state

    -- Code Block
    Block code -> push (Block code) state
    
    -- arithmetics, list and parser operations
    Tag op
        | elem op arithmeticsOps -> evalArithmetics op state
        | elem op listOps -> evalListOp op state
        | elem op parseOps -> evalParse op state

    -- Stack operations
    Tag "pop" -> pop state 
    Tag "dup" -> dup state
    Tag "swap" -> swap state 

    -- IO Operations
    Tag "print" -> printOp state 
    Tag "read" -> readOp state

    -- Executes a block
    Tag "exec" -> execBlock state
    
    -- Function and variable assignment
    Tag ":=" -> pure $ insertDict state
    Tag "fun" -> pure $ insertDict state

    -- Function call, or push 
    Tag sym -> 
        case Map.lookup sym dict of
            Just (Block body) -> evalProgram body state
            Just value -> push value state   
            Nothing  -> push (Tag sym) state
                       
-- ============================= HELPER FUNCTIONS ================================    

-- EvalBag
-- Checks if there are functions or variables inside a list
evalBag :: Dictionary -> Types -> IO (Types)
evalBag dict val = case val of
                        Tag name -> case Map.lookup name dict of
                                        Just value -> pure value
                                        Nothing -> pure (Tag name) -- Nothing found in dictationy
                        other -> pure other

-- Executes a code block, which is at the top of the stack
execBlock :: EvalState -> IO(Either BprogError EvalState)
execBlock (stk,dict) = 
    case stk of
        Block code : rest -> evalProgram code (rest,dict)
        _ -> pure $ Left (RunTime ExpectedQuotation)
                            


-- Each 
-- @param: Code 
-- @param  List
-- @returns x amount of new values, based of the length of list
--
-- Will run the code on each element in the list
evalEachBlock :: [Types] -> [Types] -> EvalState -> IO (Either BprogError EvalState)
evalEachBlock code list (rest,dict) = do
        let runEach acc el =
                case acc of
                    Left err -> pure $ Left err
                    Right (s,d) -> evalProgram code (el : s,d)
        foldM runEach (Right (rest,dict)) list 

-- Map 
-- @param code
-- @param list
-- @returns same list with new properties based on code
--
-- Will run the code on each element in the list, and preserves the list
evalMapBlock :: [Types] -> [Types] -> EvalState -> IO (Either BprogError EvalState)
evalMapBlock code list (rest,dict) = do
                let evalOne el = evalProgram code (el : rest,dict) -- el: element in the list

                result <- mapM evalOne list -- result: [Right ([2],dict)]
                case sequence result of     -- result: Right [([2],dict)]
                    Left err -> pure $ Left err
                    Right states -> case traverse extractTop states of  
                                        Nothing -> pure $ Left (RunTime ExpectedQuotation)
                                        Just newValues -> push (Bag newValues) (rest,dict)
                where
                    extractTop (x:_,_) = Just x
                    extractTop _ = Nothing

-- Foldl
-- @param code
-- @param Numbo (integer)
-- @param list
--
-- will accumulate a result based on the operation on the list
evalFoldlBlock :: [Types] -> Types -> [Types] -> EvalState -> IO (Either BprogError EvalState)
evalFoldlBlock op n list (rest,dict) = do
    let runFold acc [] = push acc (rest,dict) 
        runFold acc (el:els) = do
            result <- evalProgram op (el : acc : [], dict)
            case result of
                Left err -> pure $ Left err
                Right (newS, _) ->
                    case newS of
                        (top:_) -> runFold top els
                        _ -> pure $ Left (RunTime ExpectedList)

    
    runFold n list

-- Loop
-- @param breakCond
-- @param code
--
-- Executes the code in a loop until condition is TRUE
evalLoopBlock :: [Types] -> [Types] -> EvalState -> IO (Either BprogError EvalState)
evalLoopBlock breakCond code (rest,dict) = do
                -- evaluate the break code to be boolean operation
                let runLoop (s,d) = do
                        condResult <- evalProgram breakCond (s,d)
                        case condResult of
                            Left err -> pure $ Left err
                            Right (s',d') -> case s' of
                                (Truthy True : s'') -> pure $ Right (s'',d') -- breaks loop
                                (Truthy False : s'') -> do                   -- runs loop
                                    bodyResult <- evalProgram code (s'',d')
                                    case bodyResult of
                                        Left err -> pure $ Left err
                                        Right newState -> runLoop newState           
                                _ -> pure $ Left (RunTime ExpectedBool)    -- expects a boolean 
                runLoop (rest,dict)

-- Times
-- @param Code
-- @param Numbo n (Integer)
--
-- Loops through a code n amount of times
evalTimesBlock :: [Types] -> Integer -> EvalState -> IO (Either BprogError EvalState) 
evalTimesBlock code count (rest,dict) = do
                let runNTimes 0 state' = pure $ Right state'
                    runNTimes i state' = do
                        result <- evalProgram code state'
                        case result of
                            Left err -> pure $ Left err
                            Right newState -> runNTimes (i-1) newState
                runNTimes count (rest,dict)
