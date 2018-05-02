{-# LANGUAGE FlexibleInstances #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Data.EditSpec where

import Data.Edit
import Test.QuickCheck

instance Show (Integer -> Integer) where
  show _ = "f :: Integer -> Integer"

prop_EditApplicativeIdentity :: Edit Integer -> Bool
prop_EditApplicativeIdentity v = (pure id <*> v) == v

prop_EditApplicativeComposition
  :: Edit (Integer -> Integer) -> Edit (Integer -> Integer) -> Edit Integer -> Bool
prop_EditApplicativeComposition u v w = (pure (.) <*> u <*> v <*> w) == ((v <*> w))
