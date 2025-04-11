module Errors (
    BprogError(..),
    ProgramError(..),
    ParserError(..),
    prettyErr
) where

data BprogError 
    = RunTime ProgramError
    | Parse ParserError
    deriving (Eq, Show)

-- | Represents program execution errors.
data ProgramError 
   = StackEmpty 
   | UnknownSymbol
   | ExpectedBool
   | ExpectedBoolOrNumber
   | ExpectedEnumerable
   | ExpectedQuotation
   | ExpectedList
   | ExpectedVariable
   | DivisionByZero
   | ProgramFinishedWithMultipleValues
   | NumberConversionError
   deriving (Eq, Show)

-- | Represents parser errors.
data ParserError 
   = IncompleteString 
   | IncompleteList
   | IncompleteQuotation
   deriving(Eq, Show)

prettyErr :: BprogError -> String
prettyErr (RunTime err) = case err of
    StackEmpty -> "ERROR RUNTIME: Stack is empty"
    UnknownSymbol -> "ERROR RUNTIME: Unknown symbol"
    ExpectedBool -> "ERROR RUNTIME: Expected a boolean value"
    ExpectedBoolOrNumber -> "ERROR RUNTIME: Expected a boolean or a number"
    ExpectedEnumerable -> "ERROR RUNTIME: Expected a enumerable"
    ExpectedQuotation -> "ERROR RUNTIME: Expected a quatation block"
    ExpectedList -> "ERROR RUNTIME: Expected a list"
    ExpectedVariable -> "ERROR RUNTIME: Expected a variable"
    DivisionByZero -> "ERROR RUNTIME: Division by zero"
    ProgramFinishedWithMultipleValues -> "ERROR RUNTIME: Stack is not empty after execution"
    NumberConversionError -> "ERROR RUNTIME: Could not convert value"



prettyErr (Parse err) = case err of
    IncompleteString -> "ERROR PARSING: Unterminated string."
    IncompleteList -> "ERROR PARSING: Unterminated list."
    IncompleteQuotation -> "ERROR PARSING: Missing closing bracket."

