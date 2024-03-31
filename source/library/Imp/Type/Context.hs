module Imp.Type.Context where

import qualified Control.Monad as Monad
import qualified Control.Monad.Catch as Exception
import qualified Data.Map as Map
import qualified Imp.Exception.ShowHelp as ShowHelp
import qualified Imp.Exception.ShowVersion as ShowVersion
import qualified Imp.Type.Alias as Alias
import qualified Imp.Type.Config as Config
import qualified Imp.Type.Package as Package
import qualified Imp.Type.PackageName as PackageName
import qualified Imp.Type.Source as Source
import qualified Imp.Type.Target as Target

data Context = Context
  { aliases :: Map.Map Target.Target Source.Source,
    packages :: Map.Map Target.Target PackageName.PackageName
  }
  deriving (Eq, Show)

fromConfig :: (Exception.MonadThrow m) => Config.Config -> m Context
fromConfig config = do
  Monad.when (Config.help config) $ Exception.throwM ShowHelp.new
  Monad.when (Config.version config) $ Exception.throwM ShowVersion.new
  pure
    Context
      { aliases = Alias.toMap $ Config.aliases config,
        packages = Package.toMap $ Config.packages config
      }
