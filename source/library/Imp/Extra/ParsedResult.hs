module Imp.Extra.ParsedResult where

import qualified GHC.Plugins as Plugin

overModule ::
  (Plugin.HsParsedModule -> Plugin.HsParsedModule) ->
  Plugin.ParsedResult ->
  Plugin.ParsedResult
overModule f x = x {Plugin.parsedResultModule = f $ Plugin.parsedResultModule x}
