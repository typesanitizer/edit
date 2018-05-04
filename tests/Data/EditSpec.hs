{-# LANGUAGE ViewPatterns #-}

module Data.EditSpec where

import Data.Edit
import Test.QuickCheck.Function

prop_EditApplicativeIdentity :: Edit Int -> Bool
prop_EditApplicativeIdentity v = (pure id <*> v) == v

prop_EditApplicativeComposition
  :: Edit (Fun Int Int) -> Edit (Fun Int Int) -> Edit Int -> Bool
prop_EditApplicativeComposition (fmap apply -> u) (fmap apply -> v) w
  = (pure (.) <*> u <*> v <*> w) == (u <*> (v <*> w))
