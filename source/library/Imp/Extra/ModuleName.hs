module Imp.Extra.ModuleName where

import qualified Control.Monad.Catch as Exception
import qualified GHC.Plugins as Plugin
import qualified Imp.Exception.InvalidModuleName as InvalidModuleName
import qualified Imp.Extra.ReadP as ReadP

fromString :: (Exception.MonadThrow m) => String -> m Plugin.ModuleName
fromString x =
  maybe (Exception.throwM $ InvalidModuleName.new x) pure $
    ReadP.run Plugin.parseModuleName x
