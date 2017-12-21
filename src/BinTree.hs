module BinTree where

data BinTree a = Tree (BinTree a) (BinTree a) | Leave a
  deriving (Show, Eq, Ord)

reverseTree :: BinTree a -> BinTree a
reverseTree (Leave a) = Leave a
reverseTree (Tree l r) = Tree (reverseTree r) (reverseTree l)

depth :: BinTree a -> Int
depth (Leave a) = 1
depth (Tree l r) = 1 + max (depth l) (depth r)
