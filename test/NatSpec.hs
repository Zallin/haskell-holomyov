module NatSpec where

import Test.Hspec
import Nat

main :: IO()
main = hspec spec

spec :: Spec
spec = do
  describe "besides test" $ do
    it "returns true on adjacent numbers" $
      beside Zero (Succ Zero) `shouldBe` True
    it "returns true on adjacent numbers in reverse" $
      beside (Succ Zero) Zero `shouldBe` True
    it "returns false on zeros" $
      beside Zero Zero `shouldBe` False
    it "returns false on equal numbers" $
      beside (Succ Zero) (Succ Zero) `shouldBe` False
    it "returns false on numbers with distance more than one" $
      beside Zero (Succ (Succ Zero)) `shouldBe` False
  describe "besides2 test" $ do
    it "returns true on numbers with distance two" $
      beside2 (Succ (Succ Zero)) Zero `shouldBe` True
    it "returns true on numbers with distance two in reverse" $
      beside2 Zero (Succ (Succ Zero)) `shouldBe` True
    it "returns false on numbers with distance one" $
      beside2 Zero (Succ Zero) `shouldBe` False
    it "returns false on zeros" $
     beside2 Zero Zero `shouldBe` False
    it "returns false on equal numbers" $
     beside2 (Succ Zero) (Succ Zero) `shouldBe` False
