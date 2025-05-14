module Interpreter.ListOp (
    listOps,
    evalListOp,
) where

-- OPERATIONS ON LIST, CODE BLOCK, STRING

import Bprog.Types
import Bprog.Errors
import Interpreter.StackOp

-- Allowed operations (each, map and foldl is moved to interpreter.hs)
listOps :: [String]
listOps = ["head", "tail", "empty", "length", "cons", "append"]

evalListOp :: String -> EvalState -> IO (Either BprogError EvalState)

-- Head operation
--
-- Takes the first values of a List, Block or a String
evalListOp "head" (x:xs,dict) = 
        case x of
            Bag (h:_) -> push h (xs,dict)
            Block (h:_) -> push h (xs,dict)
            Wordsy (h:_) -> push (Wordsy [h]) (xs,dict)
            _ -> pure $ Left (RunTime ExpectedList)

-- Tail operation
-- 
-- Removes the first element of a List, Block and a String
evalListOp "tail" (x:xs,dict) = 
    case x of 
        Bag (_:t) -> push (Bag t) (xs,dict)
        Block (_:t) -> push (Block t) (xs,dict)
        Wordsy (_:t) -> push (Wordsy t) (xs,dict)
        _ -> pure $ Left (RunTime ExpectedList) 

-- Empty operation
--
-- Boolean operaiton that checks if a List, Block or String is empty
evalListOp "empty" (x:xs,dict) =  
    case x of
        Bag l -> push (Truthy (null l)) (xs,dict)
        Block code -> push (Truthy (null code)) (xs,dict) 
        Wordsy str -> push (Truthy (null str)) (xs,dict) 
        _ -> pure $ Left (RunTime ExpectedList)

-- Length operation
-- 
-- Counts the sum of elements in a List, Block or String
evalListOp "length" (x:xs,dict) =
    case x of
        Bag l -> push (Numbo (convert l)) (xs,dict)
        Block code -> push (Numbo (convert code)) (xs,dict) 
        Wordsy str -> push (Numbo (convert str)) (xs,dict) 
        _ -> pure $ Left (RunTime ExpectedList)
    where
        convert = toInteger . length

-- Cons operation
--
-- Append a Value to the begining of a List, Block or a string
-- SPECIAL RULE:
-- adding value to a string needs to be of same value
evalListOp "cons" (x:val:xs,dict) =
    case x of
        Bag ls -> push (Bag (val:ls)) (xs,dict)
        Block code -> push (Block (val:code)) (xs,dict)
        Wordsy str -> case val of
                        Wordsy [c] -> push (Wordsy (c:str)) (xs,dict)
                        _ -> pure $ Left (RunTime ExpectedString)
        _ -> pure $ Left (RunTime ExpectedList)

-- Append operation
--
-- Adding a List, Block or String to another of the same type
evalListOp "append" (x:val:xs,dict) =
    case x of
        Bag l2 -> case val of
                    Bag l1 -> push (Bag (l1 ++ l2)) (xs,dict)  
                    _ -> pure $ Left (RunTime ExpectedList)
        Block c2 -> case val of
                    Block c1 -> push (Block (c1 ++ c2)) (xs,dict)  
                    _ -> pure $ Left (RunTime ExpectedList)
        Wordsy str2 -> case val of
                    Wordsy str1 -> push (Wordsy (str1 ++ str2)) (xs,dict)  
                    _ -> pure $ Left (RunTime ExpectedList)
        _ -> pure $ Left (RunTime ExpectedList)

evalListOp _ _ = pure $ Left (RunTime ExpectedList)







