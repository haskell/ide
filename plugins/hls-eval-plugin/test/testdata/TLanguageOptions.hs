-- Support for language options
{-# LANGUAGE ScopedTypeVariables #-}

module TLanguageOptions where

-- Language options set in the module source (ScopedTypeVariables)
-- also apply to tests so this works fine
-- >>> f = (\(c::Char) -> [c])

{- Multiple options can be set with a single `:set`

>>> :set -XMultiParamTypeClasses -XFlexibleInstances
>>> class Z a b c
-}

{-

Options apply only in the section where they are defined (unless they are in the setup section), so this will fail:

>>> class L a b c
-}

{-
Options apply to all tests in the same section after their declaration.

Not set yet:

>>> class D

Now it works:

>>>:set -XMultiParamTypeClasses
>>> class C

It still works

>>> class F
-}

{- Wrong option names are reported.
>>> :set -XWrong
-}

-- >>> :set -XQuasiQuotes
-- >>> [e||]
