module Interpreter.StackOp (
    push,
    pop,
    dup,
    swap
) where

-- Internal libs
import Bprog.Types
import Bprog.Errors

-- Abstraction for pushing value onto stack
push :: Types -> EvalState -> IO (Either BprogError EvalState)
push val (stk,dict) = pure $ Right (val : stk,dict)

-- Stack Operations

-- Removes the top values of the stack
pop :: EvalState -> IO (Either BprogError EvalState)
pop ([],_) = pure $ Left (RunTime StackEmpty)
pop (_:xs,dict) = pure $ Right (xs,dict)

-- Duplicates the top value of the stack
dup :: EvalState -> IO (Either BprogError EvalState)
dup ([],_) = pure $ Left (RunTime StackEmpty)
dup (x:xs,dict) = pure $ Right (x:x:xs,dict)

-- Swaps two elements on the top of the stack
swap :: EvalState -> IO (Either BprogError EvalState)
swap (x:y:xs,dict) = pure $ Right (y:x:xs,dict)
swap (_,_) = pure $ Left (RunTime StackEmpty)
