module Types (
    Types(..),
    Stack,
    Dictionary
) where

--import Control.Monad.State
--import Control.Monad.Except
import MyMap 

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
type Dictionary = MyMap String Types


