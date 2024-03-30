module Imp.Type.Source where

import qualified Control.Monad.Catch as Exception
import qualified GHC.Plugins as Plugin
import qualified Imp.Extra.ModuleName as ModuleName

data Source
  = Implicit
  | Explicit Plugin.ModuleName
  deriving (Eq, Show)

fromModuleName :: Plugin.ModuleName -> Source
fromModuleName = Explicit

fromString :: (Exception.MonadThrow m) => String -> m Source
fromString s =
  if s == "_"
    then pure Implicit
    else fromModuleName <$> ModuleName.fromString s

isImplicit :: Source -> Bool
isImplicit s = case s of
  Implicit -> True
  Explicit _ -> False
