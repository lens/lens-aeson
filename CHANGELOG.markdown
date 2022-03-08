1.2 [????.??.??]
----------------
* Require `aeson-2.0.*` and `lens-5.0.*` or greater.
* Change the types of `_Object`, `key`, and `members`:

  ```diff
  -_Object :: Prism' t (HashMap Text Value)
  +_Object :: Prism' t (KeyMap Value)

  -key :: AsValue t => Text -> Traversal' t Value
  +key :: AsValue t => Key  -> Traversal' t Value

  -members :: AsValue t => IndexedTraversal' Text t Value
  +members :: AsValue t => IndexedTraversal' Key  t Value
  ```

  This mirrors similar changes made in `aeson-2.0.*`, where the type of
  `Object`'s field was changed from `HashMap Text Value` to `KeyMap Value`.

  The `Ixed Value` instance changes similarly:

  ```diff
  -type instance Index Value = Text
  +type instance Index Value = Key
  ```
* Add `Wrapped` and `Rewrapped` instances for `KeyMap`. These treat `KeyMap v`
  as a wrapper around `[(Key, v)]`. The order in which the key-value pairs
  appear in this list is not stable.

1.1.3 [2021.11.16]
------------------
* Drop support for pre-8.0 versions of GHC.

1.1.2 [2021.10.09]
------------------
* Allow building with `aeson-2.0.0.0`.
* Add `Index`, `IxValue`, `Ixed`, `At`, and `Each` instances for `KeyMap` if
  building with `aeson-2.0.0.0` or later.

1.1.1 [2021.02.17]
------------------
* Allow building with `lens-5.*`.
* The build-type has been changed from `Custom` to `Simple`.
  To achieve this, the `doctests` test suite has been removed in favor of using
  [`cabal-docspec`](https://github.com/phadej/cabal-extras/tree/master/cabal-docspec)
  to run the doctests.

1.1 [2019.09.26]
----------------
* Generalize the type of `_JSON` from `Prism' t a` to `Prism t t a b`. If you
  wish to continue to use the less general type, use the newly added `_JSON'`
  prism.
* Add pattern synonyms corresponding to the `Prism`s that `lens-aeson`
  provides.
* Fix the test suite on 32-bit architectures.

1.0.2
-----
* Support `doctest-0.12`

1.0.1
-----
* Revamp `Setup.hs` to use `cabal-doctest`. This makes it build
  with `Cabal-2.0`, and makes the `doctest`s work with `cabal new-build` and
  sandboxes.

1.0.0.5
----
* Fix tests to work against vector-0.11
* Documentation fixes
* No functional changes since 1.0.0.4

1.0.0.3
----
* Move lens upper bound to < 5 like the other packages in the family

1
----
* Module migrated from lens package to Data.Aeson.Lens

0.1.2
-----
* Added `members` and `values`

0.1.1
-----
* Broadened dependencies

0.1
---
* Repository initialized

