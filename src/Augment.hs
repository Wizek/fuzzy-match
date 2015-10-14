-- source: http://stackoverflow.com/a/23124701/499478

{-# LANGUAGE MultiParamTypeClasses, TypeFamilies,
  FlexibleInstances, UndecidableInstances, IncoherentInstances #-}

module Augment (augmentWith) where

class Augment a b f h where
   augmentWith :: (a -> b) -> f -> h

instance (a ~ c, h ~ b) => Augment a b c h where
   augmentWith = ($)

instance (Augment a b d h', h ~ (c -> h')) => Augment a b (c -> d) h where
   augmentWith g f = augmentWith g . f
