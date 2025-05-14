module Interpreter.Dictionary (
    handleTags
) where

-- Internal libs
import Bprog.Types

-- External libs
import qualified Data.Map as Map

handleTags :: Types -> Dictionary -> IO (Types)
handleTags (Tag sym) dict =
    case Map.lookup sym dict of
        Just val -> pure val
        Nothing -> pure (Tag sym)
handleTags val _  = pure val
