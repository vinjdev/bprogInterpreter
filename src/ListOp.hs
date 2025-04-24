module ListOp (
    listOps,
    evalListOp,
) where

import Types
import Errors

listOps :: [String]
listOps = ["head", "tail", "empty", "length", "cons", "append"]

evalListOp :: String -> EvalState -> IO (Either BprogError EvalState)

-- Head operation
evalListOp "head" (Bag (x:_): rest,dict) = pure $ Right (x : rest,dict)
evalListOp "head" (Bag [] : _,_)        = pure $ Left  (RunTime ExpectedList)

evalListOp "head" (Wordsy (c:_): rest,dict) = pure $ Right (Wordsy [c] : rest,dict)
evalListOp "head" (Wordsy [] : _,_)        = pure $ Left  (RunTime ExpectedList)

-- Tail operation
evalListOp "tail" (Bag (_:xs) : rest,dict) = pure $ Right (Bag (xs):rest,dict)
evalListOp "tail" (Bag [] : _,_) = pure $ Left (RunTime ExpectedList)

evalListOp "tail" (Wordsy (_:xs) : rest,dict) = pure $ Right (Wordsy(xs):rest,dict)
evalListOp "tail" (Wordsy [] : _,_) = pure $ Left (RunTime ExpectedList)

-- Empty operation
evalListOp "empty" (Bag xs : rest,dict) = pure $ Right (Truthy (null xs):rest,dict)
evalListOp "empty" (Wordsy xs : rest,dict) = pure $ Right (Truthy (null xs):rest,dict)

-- Length operations
evalListOp "length" (Bag xs : rest,dict) = pure $ Right (Numbo(convert xs):rest,dict)
    where
        convert = toInteger . length

evalListOp "length" (Wordsy xs : rest,dict) = pure $ Right (Numbo(convert xs):rest,dict)
    where
        convert = toInteger . length


evalListOp "cons" (Bag xs : val : rest,dict) = pure $ Right (Bag (val:xs) : rest,dict)
evalListOp "cons" (Wordsy xs : Wordsy [c] : rest,dict) = pure $ Right (Wordsy (c:xs) : rest,dict)
evalListOp "cons" (Wordsy _ : _ : _,_) = pure $ Left (RunTime ExpectedList) 

evalListOp "append" (Bag l2 : Bag l1 : rest,dict) = pure $ Right (Bag (l1 ++ l2) : rest,dict)
evalListOp "append" (Wordsy l2 : Wordsy l1 : rest,dict) = pure $ Right (Wordsy (l1 ++ l2) : rest,dict)

evalListOp _ _ = pure $ Left (RunTime ExpectedList)



