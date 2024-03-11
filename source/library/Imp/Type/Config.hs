module Imp.Type.Config where

import qualified Control.Monad as Monad
import qualified Control.Monad.Catch as Exception
import qualified Imp.Type.Alias as Alias
import qualified Imp.Type.Flag as Flag

data Config = Config
  { aliases :: [Alias.Alias],
    help :: Bool,
    version :: Bool
  }
  deriving (Eq, Show)

initial :: Config
initial =
  Config
    { aliases = [],
      help = False,
      version = False
    }

fromFlags :: (Exception.MonadThrow m) => [Flag.Flag] -> m Config
fromFlags = Monad.foldM applyFlag initial

applyFlag :: (Exception.MonadThrow m) => Config -> Flag.Flag -> m Config
applyFlag config flag = case flag of
  Flag.Alias string -> do
    alias <- Alias.fromString string
    pure config {aliases = alias : aliases config}
  Flag.Help bool -> pure config {help = bool}
  Flag.Version bool -> pure config {version = bool}
