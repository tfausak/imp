module Imp.Extra.HsParsedModule where

import qualified GHC.Hs as Hs
import qualified GHC.Plugins as Plugin
import qualified Imp.Ghc as Ghc

overModule ::
  (Plugin.Located Ghc.HsModulePs -> Plugin.Located Ghc.HsModulePs) ->
  Plugin.HsParsedModule ->
  Plugin.HsParsedModule
overModule f x = x {Hs.hpm_module = f $ Hs.hpm_module x}
