module Week1Spec (spec) where

import Test.Hspec
import Week1

spec :: Spec
spec = do
  describe "toDigits" $ do
    it "converts 1234 to [1, 2, 3, 4]" $
      toDigits 1234 `shouldBe` [1, 2, 3, 4]
    it "converts 0 to []" $
      toDigits 0 `shouldBe` []
    it "converts -17 to []" $
      toDigits (-17) `shouldBe` []

  describe "toDigitsRev" $ do
    it "converts 1234 to [4, 3, 2, 1]" $
      toDigitsRev 1234 `shouldBe` [4, 3, 2, 1]
    it "converts 0 to []" $
      toDigitsRev 0 `shouldBe` []
    it "converts -17 to []" $
      toDigitsRev (-17) `shouldBe` []
