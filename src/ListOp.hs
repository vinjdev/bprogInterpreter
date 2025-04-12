module ListOp (
    evalListOp
) where

import Types
import Errors

evalListOp :: String -> EvalState -> Either BprogError EvalState
evalListOp "head" (Bag (x:_): rest,env) = Right (x : rest,env)
evalListOp "head" (Bag [] : _,_)        = Left  (RunTime ExpectedList)

evalListOp _ _ = Left (RunTime ExpectedList)
