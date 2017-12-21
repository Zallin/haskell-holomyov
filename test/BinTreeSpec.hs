module BinTreeSpec where

import Test.Hspec
import BinTree

main :: IO()
main = hspec spec

spec :: Spec
spec = do
  describe "reverse test" $ do
    it "works on leave" $
      reverseTree (Leave 1) `shouldBe` (Leave 1)
    it "works on tre with two leaves" $
      reverseTree (Tree (Leave 1) (Leave 2)) `shouldBe` Tree (Leave 2) (Leave 1)
    it "works on tree with 2 subtrees" $
      reverseTree (Tree (Tree (Leave 1) (Leave 2)) (Tree (Leave 3) (Leave 4))) `shouldBe` Tree (Tree (Leave 4) (Leave 3)) (Tree (Leave 2) (Leave 1))
    it "works on tree with subtree and leave" $
      reverseTree (Tree (Tree (Leave 1) (Leave 2)) (Leave 3)) `shouldBe` Tree (Leave 3) (Tree (Leave 2) (Leave 1))
  describe "depth test" $ do
    it "returns 1 on leave" $
      depth (Leave 1) `shouldBe` 1
    it "returns 3 on tree with subtree and leave" $
      depth (Tree (Tree (Leave 1) (Leave 2)) (Leave 3)) `shouldBe` 3
    it "returns returns 4 on two tree with two subtrees of different size" $
      depth (Tree (Tree (Leave 1) (Leave 2)) (Tree (Tree (Leave 3) (Leave 4)) (Leave 6))) `shouldBe` 4
  describe "leaves test" $ do
    it "returns one element list on leave" $
      leaves (Leave 1) `shouldBe` [1]
    it "works on tree with two leaves" $
       leaves (Tree (Leave 1) (Leave 2)) `shouldBe` [1, 2]
    it "works on tree with subtree and leave" $
      leaves (Tree (Tree (Leave 1) (Leave 2)) (Leave 3)) `shouldBe` [1, 2, 3]
    it "works on tree with two subtrees" $
      leaves (Tree (Tree (Leave 1) (Leave 2)) (Tree (Leave 3) (Leave 4))) `shouldBe` [1, 2, 3, 4]
