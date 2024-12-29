module Imp.Extra.HsModule where

import qualified GHC.Hs as Hs

overImports ::
  ([Hs.LImportDecl Hs.GhcPs] -> [Hs.LImportDecl Hs.GhcPs]) ->
  Hs.HsModule Hs.GhcPs ->
  Hs.HsModule Hs.GhcPs
overImports f x = x {Hs.hsmodImports = f $ Hs.hsmodImports x}
