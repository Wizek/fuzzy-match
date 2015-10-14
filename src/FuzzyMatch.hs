module FuzzyMatch where

import Data.Char
import ComposeLTR

matchBonus = 1
consecutiveBonusIncrement = 1
capitalBonus = 3
boundaryBonus = 4

matchScore = matchScore' False 0

matchScore' _ _ _ "" = 0
matchScore' _ _ "" _ = 0
matchScore' wasBund consBonus (t:erm) (s:tring)
  | isMatch   = bonuses + matchScore' isBound nextConsBonus erm tring
  | otherwise = matchScore' isBound 0 (t:erm) tring
  where
  bonuses = matchBonus + maybeCapital + consBonus + maybeBound
  maybeBound = if wasBund then boundaryBonus else 0
  isMatch = (toLower t) == (toLower s)
  maybeCapital = if isUpper s then capitalBonus else 0
  nextConsBonus = consBonus + consecutiveBonusIncrement
  isBound = (s $> isAlphaNum $> not)

matches term string = (matchScore term string) > 0
