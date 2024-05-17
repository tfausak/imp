module Imp.Extra.HsParsedModule where

import qualified GHC.Hs as Hs
import qualified GHC.Plugins as Plugin

overModule ::
  (Functor f) =>
  (Plugin.Located (Hs.HsModule Hs.GhcPs) -> f (Plugin.Located (Hs.HsModule Hs.GhcPs))) ->
  Plugin.HsParsedModule ->
  f Plugin.HsParsedModule
overModule f x = (\y -> x {Hs.hpm_module = y}) <$> f (Hs.hpm_module x)
