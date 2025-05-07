module Bprog.Parser (
    tokenizer,
    parseTokens
) where

-- Parser
-- This module handles all the parsing logic
-- Reads string input, and apply the bprog data types to it

-- Internal libs
import Bprog.Types
import Bprog.Errors

-- External libs
import Text.Read (readMaybe)

-- | Tokenizer
--
-- >>> tokenizer "{ 1 2 + } [ 3 4 ]"
-- ["{","1","2","+","}","[","3","4","]"]
tokenizer :: String -> [String]
tokenizer = words

-- | ParseTokens
-- parses string input into bprog types. This is not a stack, it only assigns
-- data to data types, which has yet to be evaluated
--
-- >>> parseTokens ["{", "1", "2", "+","}", "[","3","4","]"]
-- Right [Block [Numbo 1,Numbo 2,Tag "+"],Bag [Numbo 3,Numbo 4]]
--
-- >>> parseTokens ["\"","hello","\""]
-- Right [Wordsy "hello"]
parseTokens :: [String] -> Either BprogError [Types] 
parseTokens [] = Right [] 
parseTokens (tok:toks) 
    -- Code Block { ... }
    | tok == "{" = do
        (body, rest) <- parseBracket "{" "}" toks -- Remove the brackets 
        block <- parseTokens body                 -- Returns a List of type Block
        restParsed <- parseTokens rest            -- Does the rest of the list
        Right (Block block : restParsed)
    -- List [ ... ]
    | tok == "[" = do
        (body, rest) <- parseBracket "[" "]" toks
        list <- parseTokens body
        restParsed <- parseTokens rest
        Right (Bag list : restParsed)
   -- String  " ... "
    | tok == "\"" = do
        (str, rest) <- parseString toks
        restParsed <- parseTokens rest
        Right (Wordsy str : restParsed)
   -- Literals and symbols 
    | otherwise = do
        restParsed <- parseTokens toks
        Right (parseLiteral tok : restParsed)


-- | parseBracket
--
-- >>> parseBracket "{" "}" ["\"","this","is","a","string","\"","}","1","2"]
-- Right (["\"","this","is","a","string","\""],["1","2"])
--
parseBracket :: String -> String -> [String] -> Either BprogError ([String],[String])
parseBracket open close = go 1 [] -- 1 as first bracket is omited
    where
        go _ _ [] = Left (Parse IncompleteQuotation)
        go depth acc (tok:toks)
            | tok == open = go (depth + 1) (acc ++ [tok]) toks
            | tok == close = if depth == 1
                             then Right (acc, toks)
                             else go (depth - 1) (acc ++ [tok]) toks -- Block within a block
            | otherwise = go depth (acc ++ [tok]) toks

-- | parseString
-- 
-- >>> parseString ["this","is","a","string","\"","1","2"]
-- Right ("this is a string",["1","2"])
parseString :: [String] -> Either BprogError (String, [String])
parseString = go []
    where 
        go _ [] = Left (Parse IncompleteString)        -- Error
        go str ("\"":rest) = Right (unwords str, rest) -- End of string, return
        go str (tok:toks) = go (str ++ [tok]) toks     -- append to string 

-- Parse
parseLiteral :: String -> Types
parseLiteral s =
    case parseInteger s of 
        Just val -> val
        Nothing -> case parseFloat s of
                      Just val -> val
                      Nothing -> case parseBool s of
                                    Just val -> val
                                    Nothing -> (Tag s)

parseInteger :: String -> Maybe Types
parseInteger s = case readMaybe s of
                    Just n -> Just (Numbo n)    
                    Nothing -> Nothing

parseFloat :: String -> Maybe Types
parseFloat s = case readMaybe s of
                    Just f -> Just (Deci f)    
                    Nothing -> Nothing

parseBool :: String -> Maybe Types
parseBool "True" = Just (Truthy True)
parseBool "False" = Just (Truthy False)
parseBool _ = Nothing

