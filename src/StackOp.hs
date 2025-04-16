module StackOp (
    push,
    pop,
    dup,
    swap
) where

import Types
import Errors

push :: Types -> EvalState -> IO (Either BprogError EvalState)
push val (stk,dict) = pure $ Right (val : stk,dict)

pop :: EvalState -> IO (Either BprogError EvalState)
pop ([],_) = pure $ Left (RunTime StackEmpty)
pop (_:xs,dict) = pure $ Right (xs,dict)

dup :: EvalState -> IO (Either BprogError EvalState)
dup ([],_) = pure $ Left (RunTime StackEmpty)
dup (x:xs,dict) = pure $ Right (x:x:xs,dict)

swap :: EvalState -> IO (Either BprogError EvalState)
swap (x:y:xs,dict) = pure $ Right (y:x:xs,dict)
swap (_,_) = pure $ Left (RunTime StackEmpty)
