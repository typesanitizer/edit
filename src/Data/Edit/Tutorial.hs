-- | This is a short tutorial describing how you can use the 'Data.Edit' module
-- to help you with writing rewriting code (!).
--
-- Some of the examples use the
-- <https://github.com/ndmitchell/uniplate Uniplate> library. If you want to
-- follow along as we proceed, you may want to supply the package flag
-- @uniplate_examples@ to be able to those bits of the code.
--
-- If you're testing inside a @cabal@ sandbox, this can be done using
--
-- > cabal configure --flags="uniplate_examples"
-- > cabal build
-- > cabal haddock
--
-- If you're using @stack@, the same can be accomplished using:
--
-- > stack build --flag=edit:uniplate_examples
-- > stack haddock --flag=edit:uniplate_examples --open edit
--
{-# LANGUAGE CPP #-}

module Data.Edit.Tutorial where
