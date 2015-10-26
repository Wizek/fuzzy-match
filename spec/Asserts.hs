{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module Asserts where

import Text.InterpolatedString.Perl6
import Test.HUnit (assertBool)
import Test.Hspec (shouldBe)
-- import Test.Hspec.HUnit


a >? b = assertBool [qq|Expected ($a >? $b)|] (a > b)

(=?) = shouldBe



{-

[a| a > b |]
[a| a == b |]

[aa|
  a == 1
  c > 2
|]


-}
