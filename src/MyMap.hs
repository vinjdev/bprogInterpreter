module MyMap (
    myInsert,
    myLookup,
    myEmpty,
    MyMap
) where

data MyMap k v = MyMap [(k,v)]
    deriving (Show,Eq)
    
myInsert :: Eq k => k -> v -> MyMap k v -> MyMap k v
myInsert key val (MyMap kvs) = MyMap ((key,val) : filter ((/= key) . fst) kvs)

myLookup :: Eq k => k -> MyMap k v -> Maybe v
myLookup key (MyMap kvs) = Prelude.lookup key kvs

myEmpty :: MyMap k v
myEmpty = MyMap []
