module FuzzyMatch where

import Data.Char
import ComposeLTR
import Augment
import Data.List (sortBy)
import Data.Ord (comparing, compare)

matchBonus = 1
consecutiveBonusIncrement = 1
capitalBonus = 3
boundaryBonus = 4
veryBeginningBonus = 2

matchScore = matchScoreStrict

matchSearch = matchSearchBy id
matchSearchWithScores = matchSearchWithScoresBy id

matchSearchBy fn match list = matchSearchWithScoresBy fn match list
  $> map fst

matchSearchWithScoresBy fn match list = list
  $> (map $ \x -> (x, matchScore match (fn x)))
  $> sortOn snd
  $> reverse
  $> takeWhile (snd .> (> 0))

matchScoreStrict = augmentWith go matchScoreEither
  where
  go (Left  _) = 0
  go (Right n) = n

matchScoreLax = augmentWith go matchScoreEither
  where
  go (Left  n) = n
  go (Right n) = n

matchScoreEither "" _ = Left 0
matchScoreEither term string = go 0 True veryBeginningBonus term string
  where
  go score _ _ "" _  = Right score
  go score _ _ _  "" = Left  score
  go score wasBund consBonus (t:erm) (s:tring)
    | isMatch   = go (score + bonuses) isBound nextConsBonus erm tring
    | otherwise = go score             isBound 0 (t:erm) tring
    where
    bonuses = matchBonus + maybeCapital + consBonus + maybeBound
    maybeBound = if wasBund then boundaryBonus else 0
    isMatch = (toLower t) == (toLower s)
    maybeCapital = if isUpper s then capitalBonus else 0
    nextConsBonus = consBonus + consecutiveBonusIncrement
    isBound = (s $> isAlphaNum $> not)
    -- commonPartial =

matches = augmentWith go matchScoreEither
  where
  go (Left  _) = False
  go (Right _) = True

-- @since 4.8.0.0
sortOn :: Ord b => (a -> b) -> [a] -> [a]
sortOn f =
  map snd . sortBy (comparing fst) . map (\x -> let y = f x in y `seq` (y, x))
