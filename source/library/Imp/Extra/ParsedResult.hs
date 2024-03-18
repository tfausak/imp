module Imp.Extra.ParsedResult where

import qualified GHC.Plugins as Plugin

overModule ::
  (Functor f) =>
  (Plugin.HsParsedModule -> f Plugin.HsParsedModule) ->
  Plugin.ParsedResult ->
  f Plugin.ParsedResult
overModule f x = (\y -> x {Plugin.parsedResultModule = y}) <$> f (Plugin.parsedResultModule x)
