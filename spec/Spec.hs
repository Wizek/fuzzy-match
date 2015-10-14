import Test.Hspec
import Test.QuickCheck
import Test.QuickCheck.Function
import ComposeLTR
import Asserts
import FuzzyMatch

main = hspec $ do
--   let _ = shouldBe
  describe "matches" $ do
    it "should handle trivial cases" $ do
      matches "a" "a" `shouldBe` True
      matches "a" "b" `shouldBe` False

    it "should handle empty cases" $ do
      matches "a" "" `shouldBe` False
      matches "" "" `shouldBe` False
      matches "" "a" `shouldBe` False

    it "should handle normal cases" $ do
      matches "a" "ab" `shouldBe` True
      matches "a" "ba" `shouldBe` True

    it "should match case insensitive" $ do
      matches "A" "a" `shouldBe` True
      matches "a" "A" `shouldBe` True

  describe "matchScore" $ do
    it "should rank capital letters higher than lowercase matches" $ do
      matchScore "a" "xAx" >? matchScore "a" "xax"

    it "should rank consecutive letters higher than sporadic matches" $ do
      matchScore "ab" "xabx" >? matchScore "ab" "xaxb"
