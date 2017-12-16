module NatSpec where

import Test.Hspec
import Nat

main :: IO()
main = hspec spec

one = Succ Zero
two = Succ one
three = Succ two

spec :: Spec
spec = do
  describe "besides test" $ do
    it "returns true on adjacent numbers" $
      beside Zero one `shouldBe` True
    it "returns true on adjacent numbers in reverse" $
      beside one Zero `shouldBe` True
    it "returns false on zeros" $
      beside Zero Zero `shouldBe` False
    it "returns false on equal numbers" $
      beside one one `shouldBe` False
    it "returns false on numbers with distance more than one" $
      beside Zero two `shouldBe` False
  describe "besides2 test" $ do
    it "returns true on numbers with distance two" $
      beside2 two Zero `shouldBe` True
    it "returns true on numbers with distance two in reverse" $
      beside2 Zero two `shouldBe` True
    it "returns false on numbers with distance one" $
      beside2 Zero one `shouldBe` False
    it "returns false on zeros" $
     beside2 Zero Zero `shouldBe` False
    it "returns false on equal numbers" $
     beside2 one one `shouldBe` False
  describe "sumNat test" $ do
    it "sums zeros" $
     sumNat Zero Zero `shouldBe` Zero
    it "sums with zero" $
      sumNat Zero one `shouldBe` one
    it "sums when first arg is bigger" $
      sumNat one two `shouldBe` three
    it "sums when the second arg is bigger" $
      sumNat two one `shouldBe` three
