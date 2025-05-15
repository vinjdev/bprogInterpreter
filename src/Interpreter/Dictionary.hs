module Interpreter.Dictionary (
    insertDict
) where

-- Internal libs
import Bprog.Types
import Bprog.Errors

-- External libs
import qualified Data.Map as Map

-- InsertDict
--
-- Inserts a variable or a function in the dictionary
insertDict :: EvalState -> Either BprogError EvalState
insertDict (stk,dict) = case stk of
    Tag _ : Tag _ : _ -> Left (RunTime ExpectedVariable)
    value : Tag name : rest -> Right (rest,Map.insert name value dict)
    _ -> Left (RunTime ExpectedVariable)

