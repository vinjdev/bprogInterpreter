module BprogIO (
    printOp,
    readOp
) where

import Types
import Errors

printOp :: Stack -> IO (Either BprogError Stack)
printOp (x:xs) = do
    putStrLn $ show x
    return $ Right xs
printOp [] = return $ Left (RunTime StackEmpty)

readOp :: Stack -> IO (Either BprogError Stack)
readOp stack = do
    input <- getLine
    return $ Right (Wordsy input : stack)
