module Bprog.Types (
    Types(..),
    Stack,
    Dictionary,
    EvalState
) where

import qualified Data.Map as Map

-- Data types for BPROG
data Types
    = Numbo Integer   -- Integer
    | Deci Float      -- Float 
    | Truthy Bool     -- Boolean
    | Wordsy String   -- String
    | Bag [Types]     -- List
    | Block [Types]   -- code block
    | Tag String      -- symbol
    deriving (Eq, Show)

-- Data structure for the interpreter
type Stack = [Types]

-- Data Structure for variable and function assignment
type Dictionary = Map.Map String Types

-- The current condition of the stack and dictionary
type EvalState = (Stack,Dictionary)
