module Imp.Exception.ShowVersion where

import qualified Control.Monad.Catch as Exception
import qualified Data.Version as Version
import qualified Paths_imp as This

data ShowVersion
  = ShowVersion
  deriving (Eq, Show)

instance Exception.Exception ShowVersion where
  displayException = const $ Version.showVersion This.version

new :: ShowVersion
new = ShowVersion
