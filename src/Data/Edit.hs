{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE CPP #-}

module Data.Edit where

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
-- a Writer monad where @w@ is a 'Bool'-isomorphic type with @mappend = (||)@.
--
-- For worked out examples, check the 'Data.Edit.Tutorial' module.
data Edit a
  = Clean a -- A value that represents no change.
  | Dirty a -- A value that was changed.
  deriving Eq

instance Functor Edit where
  fmap f = \case
    Clean x -> Clean (f x)
    Dirty x -> Dirty (f x)

instance Applicative Edit where
  pure = Clean
  Clean f <*> Clean x = Clean (f x)
  Clean f <*> Dirty x = Dirty (f x)
  Dirty f <*> Clean x = Dirty (f x)
  Dirty f <*> Dirty x = Dirty (f x)

instance Monad Edit where
  Clean x >>= f = f x
  Dirty x >>= f = dirty (f x)

-- | Convert the value to a 'Dirty' no matter what.
dirty :: Edit a -> Edit a
dirty = \case
  Clean x -> Dirty x
  Dirty x -> Dirty x
{-# INLINABLE dirty #-}

#ifdef WITH_COMONAD_INSTANCE
instance Comonad Edit where
  extract = \case
    Clean x -> x
    Dirty x -> x
  duplicate = \case
    Clean x -> Clean (Clean x)
    Dirty x -> Dirty (Dirty x)
#endif

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
-- @f `edits` x == fromMaybe x (f x)@
edits :: (a -> Maybe a) -> a -> Edit a
edits f x = case f x of
  Nothing -> Clean x
  Just y  -> Dirty y
