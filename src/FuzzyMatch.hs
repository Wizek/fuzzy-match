module FuzzyMatch where

import Data.Char
import ComposeLTR

matchBonus = 1
consecutiveBonusIncrement = 1
capitalBonus = 3

matchScore = matchScore' 0

matchScore' _ _ "" = 0
matchScore' _ "" _ = 0
matchScore' consBonus (t:erm) (s:tring)
  | isMatch   = bonuses + matchScore' nextConsBonus erm tring
  | otherwise = matchScore' 0 (t:erm) tring
  where
  isMatch = (toLower t) == (toLower s)
  whenMatch = if isUpper s then capitalBonus else 0
  bonuses = matchBonus + whenMatch + consBonus
  nextConsBonus = consBonus + consecutiveBonusIncrement

matches term string = (matchScore term string) > 0
