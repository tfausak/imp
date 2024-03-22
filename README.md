# Imp

[![Workflow](https://github.com/tfausak/imp/actions/workflows/workflow.yaml/badge.svg)](https://github.com/tfausak/imp/actions/workflows/workflow.yaml)
[![Hackage](https://badgen.net/hackage/v/imp)](https://hackage.haskell.org/package/imp)
[![Stackage](https://www.stackage.org/package/imp/badge/nightly?label=stackage)](https://www.stackage.org/package/imp)

Imp is a GHC plugin for automatically importing modules. This behavior is
similar to the [`-fimplicit-import-qualified`][1] flag for GHCi. In short, Imp
allows you to use fully-qualified identifiers without explicitly importing any
modules.

[1]: https://downloads.haskell.org/ghc/9.8.2/docs/users_guide/ghci.html#qualified-names

This is similar to [qualified-imports-plugin][2], but it works differently
behind the scenes and supports newer versions of GHC.

[2]: https://github.com/utdemir/qualified-imports-plugin

## Basic Usage

To use Imp, add it to your package's `build-depends`, like this:

``` cabal
library
  build-depends: imp
```

Then you can enable it with the `-fplugin=Imp` flag for GHC, like this:

``` hs
{-# OPTIONS_GHC -fplugin=Imp #-}
main = System.IO.print ()
```

For the above module, Imp will automatically import `System.IO` for you. It's
as though you wrote this instead:

``` hs
import qualified System.IO
main = System.IO.print ()
```

## Enabling Everywhere

More often than not, you'll probably want to enable Imp for an entire component
rather than for a single module. To do that, add it to your package's
`ghc-options`, like this:

``` cabal
library
  ghc-options: -fplugin=Imp
```

Then you don't need to use the `OPTIONS_GHC` pragma to enable Imp.

## Aliasing Modules

Sometimes you may want to refer to modules by an alias. For example you may
prefer using `System.IO` as simply `IO`. Imp supports this with the
`--alias=SOURCE:TARGET` option. Here's an example:

``` hs
{-# OPTIONS_GHC
  -fplugin=Imp
  -fplugin-opt=Imp:--alias=System.IO:IO #-}
main = IO.print ()
```

That is the same as writing this:

``` hs
import qualified System.IO as IO
main = IO.print ()
```

Later aliases will override earlier ones.

## Notes

Imp operates purely syntactically. It doesn't know anything about the
identifiers that you use. So if you refer to something that doesn't exist, like
`System.IO.undefined`, you'll get an error from GHC.

Imp will never insert an import for a module that you've explicitly imported.
It will only insert an import when the module is not in scope already.
