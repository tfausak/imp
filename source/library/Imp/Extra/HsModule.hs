module Imp.Extra.HsModule where

import qualified GHC.Hs as Hs
import qualified Imp.Ghc as Ghc

overDecls ::
  (Functor f) =>
  ([Hs.LHsDecl Hs.GhcPs] -> f [Hs.LHsDecl Hs.GhcPs]) ->
  Ghc.HsModulePs ->
  f Ghc.HsModulePs
overDecls f x = (\y -> x {Hs.hsmodDecls = y}) <$> f (Hs.hsmodDecls x)

overImports ::
  ([Hs.LImportDecl Hs.GhcPs] -> [Hs.LImportDecl Hs.GhcPs]) ->
  Ghc.HsModulePs ->
  Ghc.HsModulePs
overImports f x = x {Hs.hsmodImports = f $ Hs.hsmodImports x}
