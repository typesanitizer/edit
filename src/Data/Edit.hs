-- |
-- Module      :  Data.Edit
-- Copyright   :  (c) Varun Gandhi 2018
-- License     :  BSD3
--
-- Maintainer  :  theindigamer15@gmail.com
-- Stability   :  experimental
-- Portability :  portable
--
-- The 'Edit' type for working with rewriting in general, with associated
-- operations.
--
-- To see examples where the monad is used for rewriting, check the
-- 'Data.Edit.Tutorial' module.

{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}

module Data.Edit where

import Control.Applicative
import Control.Monad.Zip
import Data.Semigroup (Semigroup (..))
import GHC.Generics

#ifdef WITH_COMONAD_INSTANCE
import Control.Comonad
#endif

-- | The 'Edit' type encapsulates rewriting.
--
-- Since 'Edit' is also a monad, it allows you to easily "bubble up" information
-- on whether changes were made when working with nested data structures. This
-- is helpful when you want to save the fact that you've reaching a fixed point
-- while rewriting, instead of re-computing it after the fact using an 'Eq'
-- instance on the underlying data-type.
--
-- If you're familiar with the Writer monad, 'Edit' is morally equivalent to
-- a Writer monad where @w@ is isomorphic to 'Bool' with @(<>) = (||)@.
--
-- If you're familiar with Comonads, we provide a Comonad instance if you enable
-- the @comonad_instance@ package flag.
data Edit a
  = Clean a -- A value that represents no change.
  | Dirty a -- A value that was changed.
  deriving (Eq, Functor, Foldable, Traversable, Generic)

-- | The 'Applicative' instance m
instance Applicative Edit where
  pure = Clean
  Clean f <*> Clean x = Clean (f x)
  Clean f <*> Dirty x = Dirty (f x)
  Dirty f <*> Clean x = Dirty (f x)
  Dirty f <*> Dirty x = Dirty (f x)

instance Monad Edit where
  Clean x >>= f = f x
  Dirty x >>= f = dirty (f x)

instance Semigroup a => Semigroup (Edit a) where
  (<>) = liftA2 (<>)

instance (Semigroup a, Monoid a) => Monoid (Edit a) where
  mempty = Clean mempty
  mappend = (<>)

instance MonadZip Edit where
  mzip = liftA2 (,)

#ifdef WITH_COMONAD_INSTANCE
instance Comonad Edit where
  extract = \case
    Clean x -> x
    Dirty x -> x
  duplicate = \case
    Clean x -> Clean (Clean x)
    Dirty x -> Dirty (Dirty x)
#endif

-- | Convert the value to a 'Dirty' no matter what.
dirty :: Edit a -> Edit a
dirty = \case
  Clean x -> Dirty x
  Dirty x -> Dirty x

-- | Was an edit made? If yes, returns 'Just' otherwise 'Nothing'.
toMaybe :: Edit a -> Maybe a
toMaybe = \case
  Clean _ -> Nothing
  Dirty x -> Just x

-- | Takes a clean value and a possibly dirty value and makes an 'Edit'.
fromMaybe :: a -> Maybe a -> Edit a
fromMaybe x = \case
  Just y  -> Dirty y
  Nothing -> Clean x

-- | Takes a function that may dirty a value, and gives another which captures
-- editing semantics.
--
-- @f \`edits\` x == fromMaybe x (f x)@
edits :: (a -> Maybe a) -> a -> Edit a
edits f x = case f x of
  Nothing -> Clean x
  Just y  -> Dirty y

-- | Return 'True' iff the argument has the form @Clean _@.
isClean :: Edit a -> Bool
isClean = \case
  Clean _ -> True
  Dirty _ -> False

-- | Returns 'True' iff the argument has the form @Dirty _@.
isDirty :: Edit a -> Bool
isDirty = \case
  Clean _ -> False
  Dirty _ -> True
