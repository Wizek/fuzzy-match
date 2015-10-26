import Test.Hspec
import Test.QuickCheck
import Test.QuickCheck.Function
import ComposeLTR
import Asserts
import FuzzyMatch
import Data.Either
import qualified Data.Set as Set

main = hspec $ do
--   let _ = shouldBe
  describe "matches" $ do
    it "should handle trivial cases" $ do
      matches "a" "a" =? True
      matches "a" "b" =? False

    it "should handle empty cases" $ do
      matches "a" "" =? False
      matches "" "" =? False
      matches "" "a" =? False

    it "should handle normal cases" $ do
      matches "a" "ab" =? True
      matches "a" "ba" =? True

    it "should match case insensitive" $ do
      matches "A" "a" =? True
      matches "a" "A" =? True

  describe "matchScore" $ do
    it "should rank capital letters higher than lowercase matches" $ do
      matchScore "a" "xAx" >? matchScore "a" "xax"

    it "should rank consecutive letters higher than sporadic matches" $ do
      matchScore "ab" "xabx" >? matchScore "ab" "xaxb"

    it "should rank letters after word boundaries higher" $ do
      matchScore "a" "x a" >? matchScore "a" "xa"
      matchScore "a" "x1a" =? matchScore "a" "xa"
      matchScore "a" "ax" >? matchScore "a" "xa"

    it "should rank first-letter match higher" $ do
      matchScore "a" "ax xx" >? matchScore "a" "xx ax"

      -- Undefined
      -- matchScore "a" "x/a" >? matchScore "a" "xa"
      -- matchScore "a" "x_a" =? matchScore "a" "xa"

  describe "matchScoreEither" $ do
    it "should return Left if not all characters could be matched up" $ do
      matchScoreEither "a" "" =? Left 0
      (matchScoreEither "aa" "a" $> isLeft) =? True

    it "should return the partial match score though" $ do
      (matchScoreEither "aa" "a" $> (\(Left n)->n)) >? 0

    it "should return Left 0 if no match string is provided" $ do
      matchScoreEither "" "asd" =? Left 0

  describe "matchSearch" $ do
    it "include only the matched strings" $ do
      let matches = matchSearch "a" ["a", "b", "xa"]
      Set.fromList matches =? Set.fromList ["a", "xa"]

    it "order the matched strings by score" $ do
      let matches = matchSearch "abc" ["xabxcx", "xaxbxc", "xabcxx"]
      matches =? ["xabcxx", "xabxcx", "xaxbxc"]
