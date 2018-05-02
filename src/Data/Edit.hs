-- |
-- Module      :  Data.Edit
-- Copyright   :  (c) Varun Gandhi 2018
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  theindigamer15@gmail.com
-- Stability   :  experimental
-- Portability :  portable
--
-- The 'Edit' type for working with rewriting systems, with associated
-- operations.
--
-- To see a detailed example describing dataflow analysis in a compiler, check
-- the 'Data.Edit.Tutorial' module.

{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveAnyClass #-}

module Data.Edit
  (
  -- * Edit type and basic operations
    Edit (..)
  , fromEdit
  , isClean
  , isDirty
  , extract
  , duplicate
  , extend
  -- * Forceful conversions
  , clean
  , dirty
  -- * Conversions to and from base types
  , toMaybe
  , fromMaybe
  , edits
  , toEither
  , fromEither
  -- * Operations with lists
  , partitionEdits
  )
  where

#define MONOID_SUPERCLASS_OF_SEMIGROUP    MIN_VERSION_base(4,11,0)
#define SEMIGROUP_EXPORTED_FROM_PRELUDE   MIN_VERSION_base(4,11,0)

import Control.Applicative
import Control.DeepSeq (NFData)
import Control.Monad.Zip (MonadZip (..))
import Data.Data (Typeable, Data)
import Data.Either (partitionEithers)
import Data.Functor.Classes
import GHC.Generics (Generic)

#ifdef WITH_COMONAD_INSTANCE
import Control.Comonad
#endif
#if !SEMIGROUP_EXPORTED_FROM_PRELUDE
import Data.Semigroup (Semigroup (..))
#endif
#ifdef WITH_ARBITRARY_INSTANCE
import Test.QuickCheck (Arbitrary (..), Arbitrary1 (..)
                       , frequency, arbitrary1, shrink1)
#endif

-- | The 'Edit' type encapsulates rewriting.
--
-- Since 'Edit' is also a monad, it allows you to easily "bubble up" information
-- on whether changes were made when working with nested data structures. This
-- is helpful when you want to save the fact that you've reaching a fixed point
-- while rewriting, instead of, say re-computing it after the fact using an 'Eq'
-- instance on the underlying data-type.
--
-- For example,
--
-- >>> halveEvens x = if x `mod` 2 == 0 then (Dirty $ x `div` 2) else (Clean x)
-- >>> traverse halveEvens [1, 2, 3]
-- Dirty [1,1,3]
-- >>> traverse halveEvens [1, 3, 5]
-- Clean [1,3,5]
--
-- To support this behaviour, the 'Applicative' and 'Monad' instances have
-- "polluting" semantics:
--
-- 1. 'pure' = 'Clean' = 'return'.
-- 2. The result of '<*>' is 'Clean' if and only if both the arguments are
--    'Clean'.
-- 3. If you bind a 'Clean' value, you may get anything depending on the
--    function involved. However, if you bind a 'Dirty' value, you will
--    definitely get a 'Dirty' value back.
--
-- If you're familiar with the Writer monad, 'Edit' is morally equivalent to
-- a Writer monad where @w@ is isomorphic to 'Bool' with @(<>) = (||)@.
--
-- If you like comonads, you can use the @comonad_instance@ package flag to,
-- erm, get a legit
-- <https://hackage.haskell.org/package/comonad-5.0.3/docs/Control-Comonad.html#t:Comonad Comonad>
-- instance, instead of just having the 'extract', 'duplicate' and 'extend'
-- functions.
--
-- __Usage note:__ You probably want to import this module qualified to avoid
-- a name conflict with "Data.Maybe"'s 'Data.Maybe.fromMaybe'.

data Edit a
  = Dirty a -- ^ A value that has been modified.
  | Clean a -- ^ A value that has not been modified.
  deriving
    ( Eq, Show, Read
    , Functor, Foldable, Traversable
    , Generic, NFData, Typeable, Data
    )

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

#if MONOID_SUPERCLASS_OF_SEMIGROUP
instance Monoid a => Monoid (Edit a) where
#else
instance (Semigroup a, Monoid a) => Monoid (Edit a) where
#endif
  mempty = Clean mempty
  mappend = (<>)

instance MonadZip Edit where
  mzip = liftA2 (,)

-- These instances have been adapted from Maybe's instances.
instance Eq1 Edit where
  liftEq eq ex ey = eq (extract ex) (extract ey)

instance Show1 Edit where
  liftShowsPrec sp _ d (Clean x) = showsUnaryWith sp "Clean" d x
  liftShowsPrec sp _ d (Dirty x) = showsUnaryWith sp "Dirty" d x

instance Read1 Edit where
  liftReadPrec rp _ =
    readData (readUnaryWith rp "Clean" Clean)
    <|> readData (readUnaryWith rp "Dirty" Dirty)

#if defined(WITH_ARBITRARY_INSTANCE)
instance Arbitrary1 Edit where
  liftArbitrary arb = frequency [(1, Clean <$> arb), (3, Dirty <$> arb)]

  liftShrink shr (Dirty x) = Clean x : liftShrink shr (Clean x) ++ [Dirty x' | x' <- shr x]
  liftShrink shr (Clean x) = [Clean x' | x' <- shr x]

instance Arbitrary a => Arbitrary (Edit a) where
  arbitrary = arbitrary1
  shrink = shrink1
#endif

-- | Forcibly make the value 'Clean'.
-- You probably do not want to use this function unless you're implementing
-- some class instance for 'Edit'.
clean :: Edit a -> Edit a
clean = Clean . extract

-- | Forcibly make the value 'Dirty'.
-- You probably do not want to use this function unless you're implementing
-- some class instance for 'Edit'.
dirty :: Edit a -> Edit a
dirty = Dirty . extract

-- | Extract the final value after having done some edits.
--
-- Unlike 'Data.Maybe.Maybe''s 'Data.Maybe.fromMaybe', this function doesn't require a
-- default value for totality as both constructors have a value in them.
fromEdit :: Edit a -> a
fromEdit = \case
  Clean x -> x
  Dirty x -> x

-- | Was an edit made (is the value 'Dirty')? If yes, returns 'Just' otherwise
-- 'Nothing'.
--
-- >>> toMaybe (Clean "Good morning.")
-- Nothing
-- >>> toMaybe (Dirty "Wink, wink.")
-- Just "Wink, wink."
toMaybe :: Edit a -> Maybe a
toMaybe = \case
  Clean _ -> Nothing
  Dirty x -> Just x

-- | Takes a clean value and a possibly dirty value and makes an 'Edit'.
--
-- >>> fromMaybe "Hi" Nothing
-- Clean "Hi"
-- >>> defaultValue = 1000
-- >>> correctedValue = Just 1024
-- >>> fromMaybe defaultValue correctedValue
-- Dirty 1024
fromMaybe :: a -> Maybe a -> Edit a
fromMaybe x = \case
  Just y  -> Dirty y
  Nothing -> Clean x

-- | Takes a function that may dirty a value, and returns another which
-- saves the default value if no modification is done.
--
-- @f \`edits\` x == fromMaybe x (f x)@
edits :: (a -> Maybe a) -> a -> Edit a
edits f x = case f x of
  Just y  -> Dirty y
  Nothing -> Clean x

-- | A 'Dirty' value becomes a 'Left' and a 'Clean' value becomes a 'Right'.
--
-- Mnemonic: having things clean is usually the right situation to be in.
toEither :: Edit a -> Either a a
toEither = \case
  Dirty x -> Left x
  Clean x -> Right x

-- | A 'Left' value becomes a 'Dirty' and a 'Right' value becomes a 'Clean'.
--
-- Mnemonic: having things clean is usually the right situation to be in.
fromEither :: Either a a -> Edit a
fromEither = \case
  Left x -> Dirty x
  Right x -> Clean x

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

#if defined(WITH_COMONAD_INSTANCE)
instance Comonad Edit where
  extract = fromEdit
  duplicate = dup

instance ComonadApply Edit where
  (<@>) = (<*>)
  ( @>) = ( *>)
  (<@ ) = (<* )

#elif 1
-- | @extract = fromEdit@. Provided purely for cosmetic (aesthetic?) reasons.
extract :: Edit a -> a
extract = fromEdit

-- | Wraps the value according to its current status. Like father, like son.
duplicate :: Edit a -> Edit (Edit a)
duplicate = dup

-- | Keep track of changes while utilizing an extraction map.
--
-- > extend f = fmap f . duplicate
extend :: (Edit a -> b) -> Edit a -> Edit b
extend f = fmap f . duplicate
#endif

dup :: Edit a -> Edit (Edit a)
dup = \case
  Clean x -> Clean (Clean x)
  Dirty x -> Dirty (Dirty x)

-- | 'Dirty' values are put on the left and 'Clean' values are put on the right.
--
-- > partitionEdits = partitionEithers . map toEither
partitionEdits :: [Edit a] -> ([a], [a])
partitionEdits = partitionEithers . map toEither
