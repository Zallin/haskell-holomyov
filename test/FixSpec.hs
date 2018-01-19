module FixSpec where

import Prelude hiding (map, foldr, foldl, zip, repeat, cycle, iterate)
import Test.Hspec
import Fix

main :: IO()
main = hspec spec

spec :: Spec
spec = do
  describe "map test" $ do
    it "works on empty list" $
      map (*2) [] `shouldBe` []
    it "works on one element list" $
      map (*2) [1] `shouldBe` [2]
    it "works on multiple elements list" $
      map (*2) [2, 3, 4] `shouldBe` [4, 6, 8]
  describe "foldr test" $ do
    it "works on empty list" $
      foldr (+) 0 [] `shouldBe` 0
    it "works on one element list" $
      foldr (+) 0 [1] `shouldBe` 1
    it "works on multiple element list" $
      foldr (+) 0 [1, 2, 3, 4] `shouldBe` 10
  describe "foldl test" $ do
    it "works on empty list" $
      foldr (+) 0 [] `shouldBe` 0
    it "works on one element list" $
      foldr (+) 0 [1] `shouldBe` 1
    it "works on multiple element list" $
      foldr (+) 0 [1, 2, 3, 4] `shouldBe` 10
