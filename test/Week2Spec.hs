module Week2Spec (spec) where

import Test.Hspec
import Week2.Week2
import Week2.Log

spec :: Spec
spec = do
  describe "parseMessage" $ do
    it "should parse properly" $
      run parseMessage "E 2 562 help help" == Parsed (LogMessage (Error 2) 562 "help help") "" &&
      run parseMessage "I 29 la la la" == Parsed (LogMessage Info 29 "la la la") "" &&
      run parseMessage "This is not in the right format" == ParseError `shouldBe` True