module Week3Spec (spec) where

import Test.Hspec
import Week3.Golf

spec :: Spec
spec = do
  describe "skip" $ do
    it "should work abc" $
      skips "ABCD" == ["ABCD", "BD", "C", "D"] `shouldBe` True
    it "should work hello" $
      skips "hello!" == ["hello!", "el!", "l!", "l", "o", "!"] `shouldBe` True
    it "should work 1" $
      skips [1] == [[1]] `shouldBe` True
  
  describe "localMaxima" $ do 
    it "should work1" $
      localMaxima [2,9,5,6,1] `shouldSatisfy` (== [9,6])
    it "should work2" $
      localMaxima [2,3,4,1,5] `shouldSatisfy` (== [4])
    it "should work3" $
      localMaxima [1,2,3,4,5] `shouldSatisfy` (== [])