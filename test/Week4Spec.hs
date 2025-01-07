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

  describe "sieveSundaram" $ do 
    it "should work" $
      take (length oddPrimes) (sieveSundaram 10000000) `shouldBe` oddPrimes
      where 
        oddPrimes = [3, 5, 7, 11, 13, 17, 19, 23, 29, 31, 37, 41, 43, 47, 53, 59, 61, 67, 71, 73, 79, 83, 89, 97]
  