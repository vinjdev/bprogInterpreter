module BprogIO (
    printOp,
    readOp
) where

import Types
import Errors
import System.IO (hFlush, stdout)


printOp :: EvalState -> IO (Either BprogError EvalState)
printOp ([],_) = pure $ Left (RunTime StackEmpty)
printOp (x:xs,dict) = do
    print x
    pure $ Right (xs,dict)

readOp :: EvalState -> IO (Either BprogError EvalState)
readOp ([],_) = pure $ Left (RunTime StackEmpty)
readOp (stk,dict) = do
    putStr "Input: "
    hFlush stdout
    input <- getLine
    pure $ Right (Wordsy input : stk,dict)

