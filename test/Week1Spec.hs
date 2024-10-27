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

  describe "doubleEveryOther" $ do
    it "should work for empty lists" $
      doubleEveryOtherB [] `shouldBe` []
    it "should work for lists with one elem" $
      doubleEveryOtherB [1] `shouldBe` [2]
    it "should work" $
      doubleEveryOtherB [1,3,5,2] `shouldBe` [2,3,10,2]
    it "should work with huge list" $
      doubleEveryOtherB (take 100 (cycle [4, 2])) `shouldBe` take 100 (cycle [8,2])


