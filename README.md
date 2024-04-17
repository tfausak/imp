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
behind the scenes and supports newer versions of GHC. It is also similar to the
[implicit import proposal][3], but more limited in scope.

[2]: https://github.com/utdemir/qualified-imports-plugin
[3]: https://github.com/ghc-proposals/ghc-proposals/pull/500

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

## Aliasing Current Module

You can use the special source module name `_` (a single underscore) to refer
to the current module. This allows you to disambiguate identifiers without
referring to the current module name. For example if you want to use `This`,
you can do so with `--alias=_:This`. As a complete example, this input:

``` hs
{-# OPTIONS_GHC
  -fplugin=Imp
  -fplugin-opt=Imp:--alias=_:This #-}
module Qualified.Example where
print = putStrLn . show
defaultMain = This.print ()
```

Will effectively produce this output:

``` hs
module Qualified.Example where
print = putStrLn . show
defaultMain = Qualified.Example.print ()
```

## Recommended Usage

Combining the previous sections, the recommended usage of Imp is to enable it
in your package description (`*.cabal` file) along with any aliases that you
want in your project. For example:

``` cabal
library
  build-depends: imp ^>= 1.0.0.0
  ghc-options:
    -fplugin=Imp
    -fplugin-opt=Imp:--alias=_:This
    -fplugin-opt=Imp:--alias=Data.Map.Strict:Map
    -fplugin-opt=Imp:--alias=Data.Sequence:Seq
    -fplugin-opt=Imp:--alias=Data.Set:Set
    -- and so on ...
```

## Package Imports

It's possible that the same module name can be defined in two different
packages. In normal Haskell code, you can disambiguate using the
`PackageImports` language extension. To do the same with Imp, use the
`--package=MODULE:PACKAGE` option. For example, consider the following module:

``` hs
{-# LANGUAGE PackageImports #-}
{-# OPTIONS_GHC
  -fplugin=Imp
  -fplugin-opt=Imp:--package=Data.SemVer:semver #-}
main = print Data.SemVer.initial
```

That will produce the following output:

``` hs
{-# LANGUAGE PackageImports #-}
import qualified "semver" Data.SemVer
main = print Data.SemVer.initial
```

## Limitations

Due to limitations in how GHC plugins work, Imp cannot be used to automatically
import modules from the same compilation unit. In typical usage this means that
you cannot import modules from the same package. For example, this will not
work:

``` hs
-- A.hs
module A where
aThing = ()

-- B.hs
{-# OPTIONS_GHC -fplugin=Imp #-}
module B where
bThing = A.aThing
```

If you attempt to compile those modules in a single package, you'll get an
error like this:

```
B.hs:3:10: error: [GHC-58427]
    attempting to use module ‘example-0:A’ (A.hs) which is not loaded
  |
3 | bThing = A.aThing
  |          ^^^^^^^^
```

The only workarounds are to either import the module manually or move the
modules into separate packages. See [issue 11][] for details.

[issue 11]: https://github.com/tfausak/imp/issues/11

## Notes

Imp operates purely syntactically. It doesn't know anything about the
identifiers that you use. So if you refer to something that doesn't exist, like
`System.IO.undefined`, you'll get an error from GHC.

Imp will never insert an import for a module that you've explicitly imported.
It will only insert an import when the module is not in scope already.
