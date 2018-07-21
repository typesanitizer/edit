# Edit [![Hackage badge](https://img.shields.io/hackage/v/edit.svg?label=Hackage)](https://hackage.haskell.org/package/edit) [![Stackage badge](https://www.stackage.org/package/edit/badge/nightly?label=Stackage)](https://www.stackage.org/package/edit)

The `Edit` monad allows you to easily bubble up whether a change was made or
not when rewriting things. Some cases where this can be handy:

1. You are making a sequence of transformations on some type and want to keep
   track of whether any of them changed it or not.
2. You are rewriting a recursive type (or a garden of mutually recursive types!)
   and want to bubble up information whether something was changed or not.

For example, Reddit user /u/p__bing [says](https://www.reddit.com/r/haskell/comments/8mrqfy/ann_edit_a_small_package_for_rewriting_things/e00jo8i/?utm_content=permalink&utm_medium=front&utm_source=reddit&utm_name=haskell)

> [..] I work as an iOS developer and we have this same
> exact idea implemented, as a monad, in Swift, to make our UI updates faster
> (if a change goes through our model layer and comes out Clean, we don’t bother
> touching the UI).

A small example:

```haskell
>>> halveEvens x = if x `mod` 2 == 0 then (Dirty $ x `div` 2) else (Clean x)
>>> traverse halveEvens [1, 2, 3]
Dirty [1,1,3]
>>> traverse halveEvens [1, 3, 5]
Clean [1,3,5]
```

More thorough documentation is available on [Hackage](https://hackage.haskell.org/package/edit)
under the `Data.Edit` module. There is a tutorial too under `Data.Edit.Tutorial`.

There is also a corresponding monad transformer `EditT` available under
`Control.Monad.Trans.EditT`.

# Contributing

Please open an issue on the Github issue tracker to discuss missing documentation,
API changes etc.
