module Imp.Extra.Located where

import qualified GHC.Plugins as Plugin

overValue :: (Functor f) => (a -> f b) -> Plugin.Located a -> f (Plugin.Located b)
overValue f (Plugin.L l e) = Plugin.L l <$> f e
