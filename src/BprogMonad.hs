module BprogMonad (
    Bprog,
    runBprog,
    throwB,
    getStack,
    getDict,
    modifyStack
) where

import Types
import Errors
import MyMap

import Control.Monad.Except
import Control.Monad.State
import Control.Monad.IO.Class

data BprogState = BprogState {
    bStack :: Stack
  , bDict :: Dictionary
} deriving (Show)

-- Custom monad: IO,ERROR,STATE
newtype Bprog a = Bprog {
    unBprog :: ExceptT BprogError (StateT BprogState IO) a
} deriving (Functor,Applicative, Monad, MonadIO,MonadState BprogState, MonadError BprogError)

runBprog :: Bprog a -> Stack -> Dictionary -> IO (Either BprogError (a,BprogState))
runBprog comp stk dict = runStateT (runExceptT (unBprog comp)) (BprogState stk dict)

throwB :: BprogState -> Bprog a
throwB = throwError

getStack :: Bprog Stack
getStack = gets bStack

putStack :: Stack -> Bprog ()
putStack stk = modify $ \s -> s { bStack = stk }

getDict :: Bprog Dictionary
getDict = gets bDict

putDict :: Dictionary -> Bprog ()
putDict dict = modify $ \d -> d { bDict = dict }

modifyStack :: (Stack -> Stack) -> Bprog ()
modifyStack f = getStack >>= putStack . f
