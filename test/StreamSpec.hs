module StreamSpec where

import Test.Hspec
import Stream

main :: IO()
main = hspec spec

spec :: Spec
spec = do
  describe "head test" $ do
    it "works on nats stream" $
      shead (nats 1) `shouldBe` 1
  describe "tail test" $ do
    it "works on nats stream" $
      shead (stail (nats 1)) `shouldBe` 2
  describe "n-th test" $ do
    it "works on nats stream" $
      sn 3 (nats 1) `shouldBe` 4
  describe "take test" $ do
    it "works on nats stream" $
      stake 3 (nats 1) `shouldBe` [1, 2, 3]
  describe "map test" $ do
    it "returns powers stream on nats" $
      sn 3 (smap (\x -> x *x) (nats 1)) `shouldBe` 16
  describe "filter test" $ do
    it "filters odd values" $ do
      sn 3 (sfilter even (nats 1)) `shouldBe` 8
  describe "zip test" $ do
    it "zips nats streams" $
      sn 3 (szip (nats 1) (nats 2)) `shouldBe` (4, 5)
  describe "zip with test" $ do
    it "sums nats streams" $
      sn 3 (szipWith (+) (nats 1) (nats 2)) `shouldBe` 9
  describe "iterate test" $ do
    it "produces powers stream" $
      sn 3 (siterate (* 2) 1) `shouldBe` 8
