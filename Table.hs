-- DO NOT MODIFY THIS FILE. DO NOT SUBMIT THIS FILE.

module Table(Table(..), empty, insert, exists, lookup, delete, iterate, keys, values) where

import Prelude hiding (lookup, iterate)

-- interface
empty :: Table k v
insert :: Eq k => Table k v -> k -> v -> Table k v
exists :: Eq k => Table k v -> k -> Bool
lookup :: Eq k => Table k v -> k -> Maybe v
delete :: Eq k => Table k v -> k -> Table k v
iterate :: Table k v -> (b -> (k, v) -> b) -> b -> b
keys :: Table k v -> (b -> k -> b) -> b -> b
values :: Table k v -> (b -> v -> b) -> b -> b

-- implementation

newtype Table k v = T [(k, v)] deriving (Show)

empty = T []

insert (T t) k v = T $ insert' t k v
  where
    insert' [] k v = [(k,v)]
    insert' ((k', v'):as) k v = if k == k' then (k,v) : as
                                           else (k',v') : insert' as k v

exists (T t) k = exists' t k
  where
    exists' [] k = False
    exists' ((k', _):as) k = if k == k' then True else exists' as k

lookup (T t) k = lookup' t k
  where
    lookup' [] k = Nothing
    lookup' ((k', v):as) k = if k == k' then Just v else lookup' as k

delete (T t) k = T $ delete' t k
  where
    delete' [] _ = []
    delete' ((k', v'):as) k = if k == k' then as else (k',v') : delete' as k

iterate (T t) f d = foldl f d t

keys (T t) f d = foldl f d (map fst t)

values (T t) f d = foldl f d (map snd t)
