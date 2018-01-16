module StreamSpec where

import Test.Hspec
import Stream

main :: IO()
main = hspec spec

spec :: Spec
spec = do
  describe "stream operations test" $ do
    it "extracts head" $
      shead (nats 1) `shouldBe` 1
    it "extracts tail" $
      shead (stail (nats 1)) `shouldBe` 2
    it "takes nth element" $
      sn 3 (nats 1) `shouldBe` 4
    it "takes n elements as a list" $
      stake 3 (nats 1) `shouldBe` [1, 2, 3]
    it "maps over a stream" $
      sn 3 (smap (\x -> x *x) (nats 1)) `shouldBe` 16
    it "filters over a stream" $ do
      sn 3 (sfilter even (nats 1)) `shouldBe` 8
    it "zips streams" $
      sn 3 (szip (nats 1) (nats 2)) `shouldBe` (4, 5)
    it "zips streams with function" $
      sn 3 (szipWith (+) (nats 1) (nats 2)) `shouldBe` 9
    it "iterates over value and returns stream" $
      sn 3 (siterate (* 2) 1) `shouldBe` 8
    it "sums streams" $
      stake 3 (nats 1 + nats 2) `shouldBe` [3, 5, 7]
    it "multiplies streams" $
      stake 3 (nats 2 * nats 2) `shouldBe` [4, 9, 16]
    it "substracts streams" $
      stake 3 (nats 4 - nats 1) `shouldBe` [3, 3, 3]
