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
-- better suggestion, or find any mistakes, please let me know on the Github
-- <https://github.com/theindigamer/edit/issues issue tracker>.
--
{-# LANGUAGE CPP #-}

#ifdef TUTORIAL
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TupleSections #-}
#endif

#ifdef TUTORIAL
module Data.Edit.Tutorial
  ( -- * TL;DR
    -- $tldr

    -- * Setup
    -- $setup

    -- * Tutorial
    --
    -- $identexpr
    --
    -- $ConstFold
    --
    -- $ConstFoldTests
    --
    -- $Substitute
    --
    -- $SubstituteTests
    --
    -- $StmtConstProp
    --
    -- $ConstFoldPass
    --
    -- $ConstPropPass
    --
    -- $CombinedPass
    --
    -- $CombinedTest
    Ident (..)
    , Expr (..)
    , constFold
    , substitute
    , Stmt
    , constProp
    , constFold'
    , constFoldPass
    , substitute'
    , constProp'
    , constPropPass
    , constFoldAndPropPass
  )
  where
#else
module Data.Edit.Tutorial
  ( -- * TL;DR
    -- $tldr

    -- * Setup
    -- $setup

    -- * Tutorial
    --
    -- $identexpr
    --
    -- $ConstFold
    --
    -- $ConstFoldTests
    --
    -- $Substitute
    --
    -- $SubstituteTests
    --
    -- $CombinedPass
    --
    -- $CombinedTest
  )
  where
#endif

#ifdef TUTORIAL
import Data.Data
import Data.Edit
import Data.Generics.Uniplate.Data
import Data.List (unfoldr)
import Data.Map (Map)
import qualified Data.Map as Map

#endif

{- $tldr
Get a fixed point from applying a sequence of transformations.

> import Data.Edit (Edit, edits, polish, (>=>))
>
> mkAwesome1 :: Foo -> Maybe Foo
> ...
> mkAwesomeN :: Foo -> Maybe Foo
>
> mkAwesomeAny :: Foo -> Edit Foo
> mkAwesomeAny
>   = foldr (\f acc -> acc >=> (f `edits`)) pure
>     [mkAwesome1, ..., mkAwesomeN]
>
> mkAsAwesomeAsPossible :: Foo -> Foo
> mkAsAwesomeAsPossible = polish mkAwesomeAny

Transform a recursive data structure, keeping track of whether it was changed
or not, and feed the result to some high-level dataflow analysis function.

> import DataFlowLibrary
> import PlatedLibrary
> import Data.Edit (Edit, edits, toMaybe)
>
> instance FancyPlate Foo where ...
>
> mkAwesome :: Foo -> Maybe Foo
> mkAwesome = ...
>
> mkTotallyAwesome :: Foo -> Edit Foo
> mkTotallyAwesome = transformM (mkAwesome `edits`)
>
> dataFlowAnalysis = dataFlowLibFn (toMaybe . mkTotallyAwesome)
-}

-- $setup
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

-- $identexpr
--
-- Let's define a toy language @L@ with 'Int's and addition.
--
-- > newtype Ident = Ident String
-- >   deriving (Show, Eq)
-- >
-- > data Expr
-- >   = Val Int
-- >   | Var Ident
-- >   | Add Expr Expr
-- >   deriving (Show, Eq)

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
-- Test that the implementation works as expected.
--
-- >>> two = Add (Val 1) (Val 1)
-- >>> four = Add (Val 2) (Val 2)
-- >>> constFold (Add two four)
-- Val 6
-- >>> constFold (Add (Var "x") two)
-- Add (Var "x") (Val 2)

-- $Substitute
-- Let's say we add assignment statements to the language and write a function
-- to do constant propagation. First we add a @substitute@ function.
--
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
-- Let's test this out.
--
-- >>> x = Var (Ident "x")
-- >>> quadrupleX = Add x (Add x (Add x x))
-- >>> m1 = Map.fromList [(Ident "x", 5)]
-- >>> substitute m1 quadrupleX
-- Add (Val 5) (Add (Val 5) (Add (Val 5) (Val 5)))

-- $StmtConstProp
-- Finally add in statements and a constant propagation function.
--
-- @
-- __infix 9 :=__
-- __data Stmt = Ident := Expr__
--     __deriving (Show)__
--
-- __constProp :: Map Ident Int -> Stmt -> (Map Ident Int, Stmt)__
-- __constProp map_ (var := expr) = (f map_, var := expr')__
--   __where__
--     __expr' = substitute map_ expr__
--     __f = case expr' of__
--       __Val x -> Map.insert var x__
--       ___ -> Map.delete var__ -- delete old entry if var is re-defined
-- @
--
-- >>> x = Var (Ident "x")
-- >>> m1 = Map.fromList [(Ident "x", 5)]
-- >>> constProp m1 (Ident "y" := Var (Ident "x"))
-- (fromList [(Ident "x",5),(Ident "y",5)],Ident "y":=Val 5)
#ifdef TUTORIAL
infix 9 :=
data Stmt = Ident := Expr
  deriving (Show)

constProp :: Map Ident Int -> Stmt -> (Map Ident Int, Stmt)
constProp map_ (var := expr) = (f map_, var := expr')
  where
    expr' = substitute map_ expr
    f = case expr' of
      Val x -> Map.insert var x
      _ -> id
#endif

-- $ConstFoldPass
-- Now let's say we want to write two passes -- one for constant folding, one
-- for constant propagation, and then iterate until no more optimization can be
-- done (yes, this isn't an optimal strategy, but then this tutorial would be
-- even longer :O).
--
-- However, the 'constFold' function, as it stands, doesn't save the
-- "information" whether it changed something or not. Consequently, we won't
-- be able to tell if we hit the fixed point or not unless we do an equality
-- check (which could be expensive if the expression trees are big). Time to
-- finally use the 'Edit' monad!
--
-- We can use the 'edits' function, which converts a function @f: a -> Maybe a@
-- to a function @f' : a -> Edit a@.
--
-- @
-- __import Data.Edit__
--
-- -- We don't have to alter the core logic here, neat!
-- constFold__'__ :: Expr -> __Edit__ Expr
-- constFold__'__ = __transformM__ (go __\`edits\`__)
--  where
--   go (Add (Val i) (Val j)) = Just (Val (i + j))
--   go _ = Nothing
--
-- __constFoldPass :: [Stmt] -> Edit [Stmt]__
-- __constFoldPass ss = traverse (\\(v := e) -> (v :=) \<$\> constFold' e) ss__
-- @
--
#ifdef TUTORIAL

constFold' :: Expr -> Edit Expr
constFold' = transformM (go `edits`)
  where
    go (Add (Val i) (Val j)) = Just (Val (i + j))
    go _ = Nothing

constFoldPass :: [Stmt] -> Edit [Stmt]
constFoldPass = traverse (\(v := e) -> (v :=) <$> constFold' e)

#endif
-- $ConstPropPass
-- We also need slightly different versions of 'substitute' and 'constProp'.
-- Here we use the 'extract' function; it has the signature @Edit a -> a@.
-- It is fine to throw away the 'Clean'/'Dirty' information when we are updating
-- the map, because we are only interested in changes to the @Stmt@ and don't
-- care if the @Map@ gets changed or not.
--
-- @
-- substitute__'__ :: Map Ident Int -> Expr -> __Edit__ Expr
-- substitute__'__ m e = __transformM__ (go __\`edits\`__) e
--  where
--   go (Var x) = Val \<$\> Map.lookup x m
--   go _ = Nothing
--
-- constProp__'__ :: Map Ident Int -> Stmt -> (Map Ident Int, __Edit__ Stmt)
-- constProp__'__ map_ (var := expr) = (f map_, (var :=) __\<$\>__ expr')
--  where
--   expr' = substitute__'__ map_ expr
--   f = case __extract__ expr' of
--     Val x -> Map.insert var x
--     _ -> id
-- @
--
-- Let's add a top-level function similar to 'constFoldPass'.
--
-- Note: If you're unfamiliar with 'unfoldr', you can think of it as the
-- opposite of 'foldr'. 'foldr' takes a list and a starting value and
-- collapses it to a single value; 'unfoldr' takes a starting value (often
-- called a seed) and generates a list out of it.
--
-- @
-- __import Data.List (unfoldr)__
--
-- __constPropPass :: [Stmt] -> Edit [Stmt]__
-- __constPropPass ss = sequence $ unfoldr go (Map.empty, ss)__
--  __where__
--   __go (_, []) = Nothing__
--   __go (m, x:xs) = let (m', ex) = constProp' m x in Just (ex, (m', xs))__
-- @
#ifdef TUTORIAL

substitute' :: Map Ident Int -> Expr -> Edit Expr
substitute' m e = transformM (go `edits`) e
 where
  go (Var x) = Val <$> Map.lookup x m
  go _ = Nothing

constProp' :: Map Ident Int -> Stmt -> (Map Ident Int, Edit Stmt)
constProp' map_ (var := expr) = (f map_, (var :=) <$> expr')
 where
  expr' = substitute' map_ expr
  f = case extract expr' of
    Val x -> Map.insert var x
    _ -> Map.delete var

constPropPass :: [Stmt] -> Edit [Stmt]
constPropPass ss = sequence $ unfoldr go (Map.empty, ss)
  where
    go (_, []) = Nothing
    go (m, x:xs) = let (m', ex) = constProp' m x in Just (ex, (m', xs))

#endif
-- $CombinedPass
-- Finally putting all the pieces together. We can use the 'polish' function
-- to find the fixed point, which (in this case) is a fancy way of saying that
-- we keep iterating until we have a 'Clean' (unchanged) value.
--
-- @
-- __constFoldAndPropPass :: [Stmt] -> [Stmt]__
-- __constFoldAndPropPass = polish (constFoldPass >=> constPropPass)__
-- @
#ifdef TUTORIAL

constFoldAndPropPass :: [Stmt] -> [Stmt]
constFoldAndPropPass = polish (constFoldPass >=> constPropPass)

#endif
-- $CombinedTest
-- We're not done yet though! We still need to check that this works :P.
--
-- >>> [w, x, y] = map Ident ["w", "x", "y"]
-- >>> s1 = w := Add (Val 1) (Val 2)
-- >>> s2 = x := Add (Var w) (Var w)
-- >>> s3 = y := Add (Var w) (Add (Val 1) (Var x))
-- >>> s4 = x := Add (Var y) (Var y)
-- >>> s5 = y := Add (Var w) (Var x)
-- >>> constFoldAndPropPass [s1, s2, s3, s4, s5]
-- [Ident "w" := Val 3,Ident "x" := Val 6,Ident "y" := Val 10,Ident "x" := Val 20,Ident "y" := Val 23]
--
-- Yup, it works! For fun, let's see the transformation process in action.
-- We can do this using the 'iterations' function.
--
-- >>> pprint = putStr . unlines . map (unlines . map show)
-- >>> pprint $ iterations (constFoldPass >=> constPropPass) [s1, s2, s3, s4, s5]
--
-- The output shows the full history, with the final result that we obtained
-- earlier at the end.
--
-- @
-- Ident "w" := Add (Val 1) (Val 2)
-- Ident "x" := Add (Var (Ident "w")) (Var (Ident "w"))
-- Ident "y" := Add (Var (Ident "w")) (Add (Val 1) (Var (Ident "x")))
-- Ident "x" := Add (Var (Ident "y")) (Var (Ident "y"))
-- Ident "y" := Add (Var (Ident "w")) (Var (Ident "x"))
--
-- Ident "w" := Val 3
-- Ident "x" := Add (Val 3) (Val 3)
-- Ident "y" := Add (Val 3) (Add (Val 1) (Var (Ident "x")))
-- Ident "x" := Add (Var (Ident "y")) (Var (Ident "y"))
-- Ident "y" := Add (Val 3) (Var (Ident "x"))
--
-- Ident "w" := Val 3
-- Ident "x" := Val 6
-- Ident "y" := Add (Val 3) (Add (Val 1) (Val 6))
-- Ident "x" := Add (Var (Ident "y")) (Var (Ident "y"))
-- Ident "y" := Add (Val 3) (Var (Ident "x"))
--
-- Ident "w" := Val 3
-- Ident "x" := Val 6
-- Ident "y" := Val 10
-- Ident "x" := Add (Val 10) (Val 10)
-- Ident "y" := Add (Val 3) (Var (Ident "x"))
--
-- Ident "w" := Val 3
-- Ident "x" := Val 6
-- Ident "y" := Val 10
-- Ident "x" := Val 20
-- Ident "y" := Add (Val 3) (Val 20)
--
-- Ident "w" := Val 3
-- Ident "x" := Val 6
-- Ident "y" := Val 10
-- Ident "x" := Val 20
-- Ident "y" := Val 23
-- @
--
-- Fin.
