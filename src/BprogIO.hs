module BprogIO (
    prettyValue,
    printOp,
    readOp
) where

import Types
import Errors
import Data.List (intersperse)
import System.IO (hFlush, stdout)

prettyValue :: Types -> String
prettyValue val = case val of
    Numbo n -> show n
    Deci f -> show f
    Truthy b -> if b then "True" else "False"
    Wordsy str -> show str
    Bag list -> "[" ++ listPritty list ++ "]"
    Block code -> "{ " ++ unwords (map prettyValue code) ++ " }"
    Tag t -> t

-- Helper function for prettyValue
-- maps pretty value for getting correct format 
-- intersperse a komma ,
-- concat flattens out the list to be a string
-- [Numbo 1, Numbo 2] -> ["1", "2"] -> ["1",",","2"] -> "1,2"
listPritty :: [Types] -> String
listPritty = concat . intersperse "," . map prettyValue


printOp :: EvalState -> IO (Either BprogError EvalState)
printOp ([],_) = pure $ Left (RunTime StackEmpty)
printOp (stk@(x:_),dict) = do
    putStrLn $ prettyValue x
    pure $ Right (stk,dict)

readOp :: EvalState -> IO (Either BprogError EvalState)
readOp ([],_) = pure $ Left (RunTime StackEmpty)
readOp (stk,dict) = do
    putStr "Input: "
    hFlush stdout
    input <- getLine
    pure $ Right (Wordsy input : stk,dict)

