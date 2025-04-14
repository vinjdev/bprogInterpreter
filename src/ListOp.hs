module ListOp (
    listOps,
    evalListOp
) where

import Types
import Errors

listOps :: [String]
listOps = ["head", "tail", "empty", "length", "cons", "append", "each", "map", "foldl"]

evalListOp :: String -> EvalState -> Either BprogError EvalState
evalListOp "head" (Bag (x:_): rest,env) = Right (x : rest,env)
evalListOp "head" (Bag [] : _,_)        = Left  (RunTime ExpectedList)

evalListOp _ _ = Left (RunTime ExpectedList)
