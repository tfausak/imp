module Imp.Type.Source where

import qualified Control.Monad.Catch as Exception
import qualified GHC.Plugins as Plugin
import qualified Imp.Extra.ModuleName as ModuleName

newtype Source
  = Source Plugin.ModuleName
  deriving (Eq, Show)

fromModuleName :: Plugin.ModuleName -> Source
fromModuleName = Source

fromString :: (Exception.MonadThrow m) => String -> m Source
fromString = fmap fromModuleName . ModuleName.fromString

toModuleName :: Source -> Plugin.ModuleName
toModuleName (Source x) = x
