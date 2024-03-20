module Imp.Ghc where

import qualified GHC.Hs as Hs
import qualified GHC.Plugins as Plugin
import qualified GHC.Types.SourceText as SourceText

type HsModulePs = Hs.HsModule

newImportDecl ::
  Plugin.ModuleName ->
  Hs.ImportDecl Hs.GhcPs
newImportDecl moduleName =
  Hs.ImportDecl
    { Hs.ideclExt = Hs.noAnn,
      Hs.ideclSourceSrc = SourceText.NoSourceText,
      Hs.ideclImplicit = True,
      Hs.ideclName = Hs.noLocA moduleName,
      Hs.ideclPkgQual = Plugin.NoRawPkgQual,
      Hs.ideclSource = Plugin.NotBoot,
      Hs.ideclSafe = False,
      Hs.ideclQualified = Hs.QualifiedPre,
      Hs.ideclAs = Nothing,
      Hs.ideclHiding = Nothing
    }
