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
    it "should work True" $
      skips [True,False] == [[True,False], [False]] `shouldBe` True
    -- it "should work []" $ 
    --   skips [] == [] `shouldBe` True