module Imp.Extra.HsParsedModule where

import qualified GHC.Hs as Hs
import qualified GHC.Plugins as Plugin
import qualified Imp.Ghc as Ghc

overModule ::
  (Functor f) =>
  (Plugin.Located Ghc.HsModulePs -> f (Plugin.Located Ghc.HsModulePs)) ->
  Plugin.HsParsedModule ->
  f Plugin.HsParsedModule
overModule f x = (\y -> x {Hs.hpm_module = y}) <$> f (Hs.hpm_module x)
