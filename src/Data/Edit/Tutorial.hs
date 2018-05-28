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
-- to help you with writing dataflow analysis code for a compiler. The example
-- is a bit artificial for the sake of relative conciseness -- if you have a
-- better suggestion, please let me know on the Github issue tracker.
--
{-# LANGUAGE CPP #-}

#ifdef TUTORIAL
{-# LANGUAGE DeriveDataTypeable #-}
#endif

module Data.Edit.Tutorial
  ( -- * Too Long; Didn't Read (TL;DR)
    -- $tldr

    -- * Setup
    -- $setup

    -- * Tutorial
    -- Let's define a toy language @L@ with 'Int's and the almighty addition.
    --
    -- $identexpr
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
    -- $ConstFold
    --
    -- Quickly test that this works.
    --
    -- $ConstFoldTests
    --
    -- Let's say we add assignment statements to the language and write a function
    -- to do constant propagation. First we add a @substitute@ function.
    --
    -- $Substitute
    --
    -- Let's test this out.
    --
    -- $SubstituteTests

    module Data.Edit.Tutorial
  )
  where

-- $tldr
-- Get a fixed point from applying a sequence of transformations.
--
-- > import Control.Monad (>=>)
-- > import Data.Edit (polish, edits)
-- >
-- > mkAwesome1 :: Foo -> Maybe Foo
-- > ...
-- > mkAwesomeN :: Foo -> Maybe Foo
-- >
-- > mkAwesomeAny :: Foo -> Edit Foo
-- > mkAwesomeAny
-- >   = foldr (\f acc -> acc >=> (f `edits`)) pure
-- >     [mkAwesome1, ..., mkAwesomeN]
-- >
-- > mkAsAwesomeAsPossible :: Foo -> Foo
-- > mkAsAwesomeAsPossible = polish mkAwesomeAny
--
-- Transform a recursive data structure, keeping track of whether it was changed
-- or not, and feed the result to some high-level dataflow analysis function.

--
-- > import DataFlowLibrary
-- > import PlatedLibrary
-- > import Data.Edit (Edit, edits, toMaybe)
-- >
-- > instance FancyPlate Foo where ...
-- >
-- > mkAwesome :: Foo -> Maybe Foo
-- > mkAwesome = ...
-- >
-- > mkTotallyAwesome :: Foo -> Edit Foo
-- > mkTotallyAwesome = transformM (mkAwesome `edits`)
-- >
-- > dataFlowAnalysis = dataFlowLibFn (toMaybe . mkTotallyAwesome)

-- $setup
--
-- The examples here use the
-- <https://github.com/ndmitchell/uniplate Uniplate> and
-- <https://github.com/haskell/containers Containers> libraries.
-- If you want to
-- follow along as we proceed, you will want to supply the package flag
-- @tutorial@ and maybe read the docs in your browser.
--
-- If you're testing inside a @cabal@ sandbox, this can be done using
--
-- > cabal configure --flags="tutorial"
-- > cabal build
-- > cabal haddock
--
-- If you're using @stack@, the same can be done using:
--
-- > stack build --flag=edit:tutorial
-- > stack haddock --flag=edit:tutorial --open edit
--

#ifdef TUTORIAL

import Data.Data
import Data.Generics.Uniplate.Data
import Data.Map (Map)
import qualified Data.Map as Map

#endif

{-
-- $identexpr
--
-- > newtype Ident = Ident String
-- >   deriving (Show, Eq)
-- >
-- > data Expr
-- >   = Val Int
-- >   | Var Ident
-- >   | Add Expr Expr
-- >   deriving (Show, Eq)
-}

#ifdef TUTORIAL

newtype Ident = Ident String
  deriving (Eq, Ord, Show, Typeable, Data)

data Expr
  = Val Int
  | Add Expr Expr
  | Var Ident
  deriving (Show, Eq, Typeable, Data)

#endif
-- $ConstFold
-- @
-- __\{-\# LANGUAGE DeriveDataTypeable \#-\}__
--
-- __import Data.Data__
-- __import Data.Generics.Uniplate.Data__
--
-- newtype Ident = Ident String
--   deriving (Show, Eq__, Typeable, Data__)
--
-- data Expr
--   = Val Int
--   | Var Ident
--   | Add Expr Expr
--   deriving (Show, Eq__, Typeable, Data__)
--
-- __constFold :: Expr -> Expr__
-- __constFold e = rewrite go e__
--   __where__
--     __go (Add (Val i) (Val j)) = Just (Val (i + j))__
--     __go _ = Nothing__
-- @
#ifdef TUTORIAL

constFold :: Expr -> Expr
constFold = rewrite go
  where
    go (Add (Val i) (Val j)) = Just (Val (i + j))
    go _ = Nothing

#endif
-- $ConstFoldTests
-- >>> two = Add (Val 1) (Val 1)
-- >>> four = Add (Val 2) (Val 2)
-- >>> constFold (Add two four)
-- Val 6
-- >>> constFold (Add (Var "x") two)
-- Add (Var "x") (Val 2)

-- $Substitute
-- @
-- __import Data.Map (Map)__
-- __import qualified Data.Map as Map__
--
-- newtype Ident = Ident String
--   deriving (Eq, __Ord,__ Show, Typeable, Data)
--
-- __substitute :: Map Ident Int -> Expr -> Expr__
-- __substitute m e = rewrite go e__
--   __where__
--     __go (Var x) = Val \<$\> Map.lookup x m__
--     __go _ = Nothing__
-- @
#ifdef TUTORIAL

substitute :: Map Ident Int -> Expr -> Expr
substitute m = rewrite go
  where
    go (Var x) = Val <$> Map.lookup x m
    go _ = Nothing

#endif
-- $SubstituteTests
-- >>> x = Var (Ident "x")
-- >>> quadrupleX = Add x (Add x (Add x x))
-- >>> m1 = Map.fromList [(Ident "x", 5)]
-- >>> substitute m1 quadrupleX
-- Add (Val 5) (Add (Val 5) (Add (Val 5) (Val 5)))

-- $StmtConstProp
-- @
-- __infix 9 :=__
-- __data Stmt = Ident := Expr__
--     __deriving (Show)__
--
-- __constProp :: (Map Ident Int, Stmt) -> (Map Ident Int, Stmt)__
-- __constProp (map_, var := expr) = (f map_, var := expr')__
--   __where__
--     __expr' = substitute map_ expr__
--     __f = case expr' of__
--       __Val x -> Map.insert var x__
--       ___ -> id__
-- @
#ifdef TUTORIAL

infix 9 :=
data Stmt = Ident := Expr
  deriving (Show)

constProp :: (Map Ident Int, Stmt) -> (Map Ident Int, Stmt)
constProp (map_, var := expr) = (f map_, var := expr')
  where
    expr' = substitute map_ expr
    f = case expr' of
      Val x -> Map.insert var x
      _ -> id
#endif
