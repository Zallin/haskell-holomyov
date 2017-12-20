module BinTree where

data BinTree a = Tree (BinTree a) (BinTree a) | Leave a
  deriving (Show, Eq, Ord)

reverseTree :: BinTree a -> BinTree a
reverseTree (Leave a) = Leave a
reverseTree (Tree l r) = Tree (reverseTree r) (reverseTree l)
