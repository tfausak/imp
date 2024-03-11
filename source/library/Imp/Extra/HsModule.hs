module Imp.Extra.HsModule where

import qualified GHC.Hs as Hs
import qualified Imp.Ghc as Ghc

overImports ::
  ([Hs.LImportDecl Hs.GhcPs] -> [Hs.LImportDecl Hs.GhcPs]) ->
  Ghc.HsModulePs ->
  Ghc.HsModulePs
overImports f x = x {Hs.hsmodImports = f $ Hs.hsmodImports x}
