module Imp.Ghc where

import qualified GHC.Hs as Hs
import qualified GHC.Plugins as Plugin
import qualified GHC.Types.SourceText as SourceText

type HsModulePs = Hs.HsModule Hs.GhcPs

newImportDecl ::
  Plugin.ModuleName ->
  Hs.ImportDecl Hs.GhcPs
newImportDecl moduleName =
  Hs.ImportDecl
    { Hs.ideclExt =
        Hs.XImportDeclPass
          { Hs.ideclAnn = Hs.noAnn,
            Hs.ideclSourceText = SourceText.NoSourceText,
            Hs.ideclImplicit = True
          },
      Hs.ideclName = Hs.noLocA moduleName,
      Hs.ideclPkgQual = Plugin.NoRawPkgQual,
      Hs.ideclSource = Hs.NotBoot,
      Hs.ideclSafe = False,
      Hs.ideclQualified = Hs.QualifiedPre,
      Hs.ideclAs = Nothing,
      Hs.ideclImportList = Nothing
    }
