{-# LANGUAGE DerivingStrategies #-}

module Tree
  ( Key
  , Tree (..)
  , empty
  , insert
  ) where

import Prelude hiding (lookup)

import Data.Maybe (isJust)

-- Exercise 1.1
type Key = String

data Tree a
  = Leaf
  | Tree (Tree a) Key a (Tree a)
  deriving stock (Show)

empty :: Tree a
empty = Leaf

insert :: Key -> a -> Tree a -> Tree a
insert key val Leaf = Tree Leaf key val Leaf
insert key val (Tree l k v r)
  | key < k   = Tree (insert key val l) k v r
  | key > k   = Tree l k v (insert key val r)
  | otherwise = Tree l key val r

-- a.
member :: Key -> Tree a -> Bool
member k = isJust . lookup k

-- b.
lookup :: Key -> Tree a -> Maybe a
lookup _ Leaf = Nothing
lookup key (Tree l k v r)
  | key < k   = lookup key l
  | key > k   = lookup key r
  | otherwise = Just v

-- c.
-- I won't demonstrate (too lazy), but...
-- (a) will insert "t", "s", "p", and "i" normally, with "s" to the left of "t",
-- then "p" to the left of "t" and "s", and "i" to the left of "t", "s" and "p".
-- The next "p" will have no effect as it exists, and the tree is returned
-- unchanged. "f" and "b" are finally inserted, "f" to the left of everything,
-- and then "b" to the left of again everything. The tree remains unchanged for
-- "s" and "t" inserted next. The conclusion is that everything is always
-- inserted left, and the tree is unbalanced.
-- (b) will keep inserting on the right, so it behaves like a linked list and it
-- has O(n) for lookup. This is because after "a" is inserted, and "b" is
-- inserted as well, "b" will be greater, so to the right of "a". With "c", it
-- will be inserted to the right of both "a" and "b", and so on. Notice that in
-- this case, insertion is O(n²).

-- d.
-- TODO
