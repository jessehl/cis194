module Week2Spec (spec) where

import Test.Hspec
import Week2.Week2
import Week2.Log

spec :: Spec
spec = do
  describe "parseMessage" $ do
    it "should parse properly" $
      parseMessage "E 2 562 help help" == LogMessage (Error 2) 562 "help help" &&
      parseMessage "I 29 la la la" == LogMessage Info 29 "la la la" &&
      parseMessage "This is not in the right format" == Unknown "This is not in the right format" `shouldBe` True