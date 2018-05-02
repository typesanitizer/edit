-- |
-- Module      :  Data.Edit
-- Copyright   :  (c) Varun Gandhi 2018
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  theindigamer15@gmail.com
-- Stability   :  experimental
-- Portability :  portable
--
-- This is a short (?) tutorial describing how you can use the 'Data.Edit' module
-- to help you with writing dataflow analysis code for a compiler.
--
-- __TL;DR:__
--
-- > import DataFlowLibrary
-- > import PlatedLibrary
-- >
-- > instance FancyPlate Foo where ...
-- >
-- > mkAwesome :: Foo -> Maybe Foo
-- > mkAwesome = ...
-- >
-- > mkTotallyAwesome :: Foo -> Edit Foo
-- > mkTotallyAwesome = transformM (fromMaybe mkAwesome)
-- >
-- > dataFlowAnalysis = dataFlowLibFn (toMaybe . mkTotallyAwesome)
--
-- __Setup:__
--
-- The examples here use the
-- <https://github.com/ndmitchell/uniplate Uniplate> and
-- <https://github.com/haskell/containers Containers> libraries.
-- If you want to
-- follow along as we proceed, you will want to supply the package flag
-- @tutorial@.
--
-- If you're testing inside a @cabal@ sandbox, this can be done using
--
-- > cabal configure --flags="tutorial"
-- > cabal build
-- > cabal haddock
--
-- If you're using @stack@, the same can be accomplished using:
--
-- > stack build --flag=edit:tutorial
-- > stack haddock --flag=edit:tutorial --open edit
--
-- __Start:__
--
-- Let's define a toy language @L@ with 'Int's and the almighty addition.
--
-- > newtype Ident = Ident String
-- >   deriving (Show, Eq)
-- >
-- > data Expr
-- >   = Val Int
-- >   | Var Ident
-- >   | Add Expr Expr
-- >   deriving (Show, Eq)
--
-- Q. How would you implement constant folding for the 'Expr' type?
--
-- (1) Write the recursion by hand. While this is easy enough to do since
-- 'Expr' only has a few constructors, this isn't very practical when you have
-- lots of constructors. The exact point where you recognize that this is a
-- recursive descent into unmaintainability depends on your personal boilerplate
-- threshold.
--
-- (2) Use recursion schemes and get lost in the unfathomable type errors
-- (I'm half-joking). While this is a reasonable approach, we're not going to
-- follow this here.
--
-- (3) Use a generics library. For simplicity, we'll be using Uniplate here.
-- The particular functions that are relevant at the moment are 'rewrite' and
-- 'transform'. Let's use 'rewrite'.
--
-- @
-- __\{-\# LANGUAGE DeriveDataTypeable \#-\}__
--
-- __import Data.Data__
-- __import Data.Generics.Uniplate.Data__
--
-- newtype Ident = Ident String
--   deriving (Show, Eq, __Typeable, Data__)
--
-- data Expr
--   = Val Int
--   | Var Ident
--   | Add Expr Expr
--   deriving (Show, Eq, __Typeable, Data__)
--
-- __constFold :: Expr -> Expr__
-- __constFold e = rewrite go e__
--   __where__
--     __go (Add (Val i) (Val j)) = Just (Val (i + j))__
--     __go _ = Nothing__
-- @
--
-- For testing purposes, let use define a few expressions that we can reuse.
--
-- >>> two = Add (Val 1) (Val 1)
-- >>> four = Add (Val 2) (Val 2)
-- >>> constFold (Add two four)
-- Val 6
-- >>> constFold (Add (Var "x") two)
-- Add (Var "x") (Val 2)
--
-- Let's say you want to write a constant propagation function. That would
-- probably look something like
--
-- @
-- \{-\# LANGUAGE DeriveDataTypeable \#-\}
--
-- import Data.Data
-- import Data.Generics.Uniplate.Data
-- __import Data.Map (Map)__
-- __import qualified Data.Map as Map__
--
-- newtype Ident = Ident String
--   deriving (Show, Eq, Typeable, Data)
--
-- data Expr
--   = Val Int
--   | Var Ident
--   | Add Expr Expr
--   deriving (Show, Eq, Typeable, Data)
--
-- constFold :: Expr -> Expr
-- constFold e = rewrite go e
--   where
--     go (Add (Val i) (Val j)) = Just (Val (i + j))
--     go _ = Nothing
--
-- __constProp :: Map Ident Int -> Expr -> Expr__
-- __constProp m e = rewrite go e__
--   __where__
--     __go (Var x) = Val \<$\> Map.lookup x m__
--     __go _ = Nothing__
-- @
--
-- Let's test this out.
--
-- >>> x = Var (Ident "x")
-- >>> quadrupleX = Add (x (Add x (Add x x)))
-- >>> m1 = Map.fromList [(Ident "x", 5)]
-- >>> constProp m1 x
-- Add (Val 5) (Add (Val 5) (Add (Val 5) (Val 5)))
--
{-# LANGUAGE CPP #-}

#ifdef TUTORIAL
{-# LANGUAGE DeriveDataTypeable #-}
#endif

module Data.Edit.Tutorial where

#ifdef TUTORIAL

import Data.Data
import Data.Generics.Uniplate.Data
import Data.Map (Map)
import qualified Data.Map as Map

newtype Ident = Ident String
  deriving (Eq, Ord, Show, Typeable, Data)

data Expr
  = Val Int
  | Add Expr Expr
  | Var Ident
  deriving (Show, Eq, Typeable, Data)

constFold :: Expr -> Expr
constFold = rewrite go
  where
    go (Add (Val i) (Val j)) = Just (Val (i + j))
    go _ = Nothing

constProp :: Map Ident Int -> Expr -> Expr
constProp m = rewrite go
  where
    go (Var x) = Val <$> Map.lookup x m
    go _ = Nothing

#endif
