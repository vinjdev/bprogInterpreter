module Types (
    Types(..),
    Stack,
    Dictionary
) where

import qualified Data.Map as Map
--import Control.Monad.State
--import Control.Monad.Except
--import Errors 


-- Data types for BPROG
data Types
    = Numbo Integer   -- Integer
    | Deci Float      -- Float 
    | Truthy Bool     -- Boolean
    | Wordsy String   -- String
    | Bag [Types]     -- List
    | Block [Types]   -- code block
    | Tag String      -- symbol
    deriving(Eq, Show)

-- Data structure for the interpreter
type Stack = [Types]

-- Data Structure for variable and function assignment
type Dictionary = Map.Map String Types

{-
data BprogState = BprogState 
    {
          stack :: Stack
        , tags :: SymbolTable
    }
    deriving(Show)

-- Interpreter monad
-- Can fail with BprogError
-- Can modify and access BprogState (Stack and Dictionary)
-- Produces value a if succeded
type Interpreter a = ExceptT BprogError (State BprogState) a
-}

