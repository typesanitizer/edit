-- |
-- Module      :  Data.Edit
-- Copyright   :  (c) Varun Gandhi 2018
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  theindigamer15@gmail.com
-- Stability   :  experimental
-- Portability :  portable
--
-- A monad/comonad transformer for the 'Edit' monad.
--
-- I'm not entirely sure what this might be useful for, but it is provided for
-- the sake of completeness. If you find a concrete use case for it, please
-- submit a PR on Github to fix this section!

{-# LANGUAGE CPP #-}

module Control.Monad.Trans.Edit where

import Control.Applicative (liftA2)
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Control.Monad.Zip
import Data.Edit
import Data.Functor.Classes

#if defined(WITH_COMONAD_INSTANCE)
import Control.Comonad
import Control.Comonad.Trans.Class
#endif

newtype EditT m a = EditT { runEditT :: m (Edit a) }

instance Eq1 m => Eq1 (EditT m) where
  liftEq eq (EditT x) (EditT y) = liftEq (liftEq eq) x y

instance Show1 m => Show1 (EditT m) where
  liftShowsPrec sp sl d (EditT m) =
      showsUnaryWith (liftShowsPrec sp' sl') "EditT" d m
    where
      sp' = liftShowsPrec sp sl
      sl' = liftShowList sp sl

instance Read1 m => Read1 (EditT m) where
  liftReadsPrec rp rl = readsData $
      readsUnaryWith (liftReadsPrec rp' rl') "EditT" EditT
    where
      rp' = liftReadsPrec rp rl
      rl' = liftReadList rp rl

instance (Eq1 m, Eq a) => Eq (EditT m a) where (==) = eq1
instance (Read1 m, Read a) => Read (EditT m a) where readsPrec = readsPrec1
instance (Show1 m, Show a) => Show (EditT m a) where showsPrec = showsPrec1

mapEditT :: (m (Edit a) -> n (Edit b)) -> EditT m a -> EditT n b
mapEditT f = EditT . f . runEditT

instance Functor m => Functor (EditT m) where
    fmap f = mapEditT (fmap (fmap f))

instance Applicative m => Applicative (EditT m) where
    pure = EditT . pure . Clean
    EditT mf <*> EditT mx = EditT $ liftA2 (<*>) mf mx

instance Monad m => Monad (EditT m) where
  return = pure
  EditT x >>= f = EditT $ do
    v <- x
    case v of
      Dirty y -> dirty <$> runEditT (f y)
      Clean y -> runEditT (f y)

instance MonadTrans EditT where
  lift = EditT . fmap Clean

instance MonadIO m => MonadIO (EditT m) where
  liftIO = lift . liftIO

instance MonadZip m => MonadZip (EditT m) where
  mzip (EditT x) (EditT y) = EditT (liftA2 mzip x y)

instance Foldable f => Foldable (EditT f) where
    foldMap f (EditT a) = foldMap (foldMap f) a

instance Traversable f => Traversable (EditT f) where
    traverse f (EditT a) = EditT <$> traverse (traverse f) a

#if defined(WITH_COMONAD_INSTANCE)
instance Comonad c => Comonad (EditT c) where
  extract = extract . extract . runEditT
  duplicate (EditT cex) = EditT ceEcex
    where
      ef = case extract cex of
        Dirty _ -> Dirty
        Clean _ -> Clean
      ceEcex = fmap (ef . EditT) (duplicate cex)

instance ComonadTrans EditT where
  lower = fmap extract . runEditT
#endif
