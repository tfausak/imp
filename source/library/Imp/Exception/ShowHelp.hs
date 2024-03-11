module Imp.Exception.ShowHelp where

import qualified Control.Monad.Catch as Exception
import qualified Imp.Type.Flag as Flag
import qualified System.Console.GetOpt as GetOpt

data ShowHelp
  = ShowHelp
  deriving (Eq, Show)

instance Exception.Exception ShowHelp where
  displayException = const $ GetOpt.usageInfo "imp" Flag.options

new :: ShowHelp
new = ShowHelp
