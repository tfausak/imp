module Imp.Extra.ImportDecl where

import qualified Data.Maybe as Maybe
import qualified GHC.Hs as Hs
import qualified GHC.Plugins as Plugin

toModuleName :: Hs.ImportDecl Hs.GhcPs -> Plugin.ModuleName
toModuleName x = Plugin.unLoc . Maybe.fromMaybe (Hs.ideclName x) $ Hs.ideclAs x
