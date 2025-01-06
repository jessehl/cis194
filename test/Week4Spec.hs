module Week4Spec (spec) where

import Test.Hspec
import Week4

spec :: Spec
spec = do
  describe "fun1" $ do
    it "should work" $
      fun1 [234,234,1,0,3] `shouldSatisfy` (==fun1_ [234,234,1,0,3])

  describe "fun2" $ do 
    it "should work" $ 
      fun2 2413354 `shouldSatisfy` (== fun2_ 2413354)

  describe "xor" $  do
    it "should work" $
       xor [True, True, False, True] == True &&
       xor [] == False && 
       xor [True] == True &&
       xor [False, True, False] == True &&
       xor [False, True, False, False, True] `shouldBe` False
       
