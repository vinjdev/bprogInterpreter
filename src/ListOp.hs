module ListOp (
    listOps,
    evalListOp,
) where

import Types
import Errors

listOps :: [String]
listOps = ["head", "tail", "empty", "length", "cons", "append"]

evalListOp :: String -> EvalState -> IO (Either BprogError EvalState)
evalListOp "head" (Bag (x:_): rest,dict) = pure $ Right (x : rest,dict)
evalListOp "head" (Bag [] : _,_)        = pure $ Left  (RunTime ExpectedList)

evalListOp "tail" (Bag (_:xs) : rest,dict) = pure $ Right (Bag (xs):rest,dict)
evalListOp "tail" (Bag [] : _,_) = pure $ Left (RunTime ExpectedList)

evalListOp "empty" (Bag xs : rest,dict) = pure $ Right (Truthy (null xs):rest,dict)

evalListOp "length" (Bag xs : rest,dict) = pure $ Right (Numbo(convert xs):rest,dict)
    where
        convert = toInteger . length

evalListOp "cons" (Bag xs : val : rest,dict) = pure $ Right (Bag (val:xs) : rest,dict)

evalListOp "append" (Bag l2 : Bag l1 : rest,dict) = pure $ Right (Bag (l1 ++ l2) : rest,dict)

evalListOp _ _ = pure $ Left (RunTime ExpectedList)



