module Imp.Extra.HsModule where

import qualified GHC.Hs as Hs

overDecls ::
  (Functor f) =>
  ([Hs.LHsDecl Hs.GhcPs] -> f [Hs.LHsDecl Hs.GhcPs]) ->
  Hs.HsModule Hs.GhcPs ->
  f (Hs.HsModule Hs.GhcPs)
overDecls f x = (\y -> x {Hs.hsmodDecls = y}) <$> f (Hs.hsmodDecls x)

overImports ::
  ([Hs.LImportDecl Hs.GhcPs] -> [Hs.LImportDecl Hs.GhcPs]) ->
  Hs.HsModule Hs.GhcPs ->
  Hs.HsModule Hs.GhcPs
overImports f x = x {Hs.hsmodImports = f $ Hs.hsmodImports x}
