module Imp.Type.Target where

import qualified Control.Monad.Catch as Exception
import qualified GHC.Plugins as Plugin
import qualified Imp.Extra.ModuleName as ModuleName

newtype Target
  = Target Plugin.ModuleName
  deriving (Eq, Ord, Show)

fromModuleName :: Plugin.ModuleName -> Target
fromModuleName = Target

fromString :: (Exception.MonadThrow m) => String -> m Target
fromString = fmap fromModuleName . ModuleName.fromString

toModuleName :: Target -> Plugin.ModuleName
toModuleName (Target x) = x
