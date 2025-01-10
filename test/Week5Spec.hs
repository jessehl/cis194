module Week5Spec (spec) where

import Test.Hspec
import Week5.Week5
import Week5.ExprT

spec :: Spec
spec = do
  describe "eval" $ do
    it "should work" $
      eval (Mul (Add (Lit 2) (Lit 3)) (Lit 4)) `shouldSatisfy` (== 20)