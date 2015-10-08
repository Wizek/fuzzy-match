import Test.Hspec
import Test.QuickCheck
import Test.QuickCheck.Function
import ComposeLTR

matchScore _ [] = 0
matchScore (t:erm) (s:tring)
  | t == s    = 1 + matchScore erm tring
  | otherwise = matchScore erm tring

matches term string = (matchScore term string) > 0

main = hspec $ do
  describe "" $ do
    it "should" $ do
      matches "a" "a" `shouldBe` True
      matches "a" "b" `shouldBe` False