module Week4Spec (spec) where

import Test.Hspec
import Week4

spec :: Spec
spec = do
  describe "fun1" $ do
    it "should work" $
      fun1 [234,234,1,0,3] `shouldSatisfy` (==fun1_ [234,234,1,0,3] )
