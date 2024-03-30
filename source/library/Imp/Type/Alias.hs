module Imp.Type.Alias where

import qualified Control.Monad as Monad
import qualified Control.Monad.Catch as Exception
import qualified Data.Map as Map
import qualified GHC.Plugins as Plugin
import qualified Imp.Exception.InvalidAlias as InvalidAlias
import qualified Imp.Extra.ModuleName as ModuleName
import qualified Imp.Type.Target as Target

data Alias = Alias
  { source :: Plugin.ModuleName,
    target :: Target.Target
  }
  deriving (Eq, Show)

fromString :: (Exception.MonadThrow m) => String -> m Alias
fromString string = do
  let (before, after) = break (== ':') string
  Monad.when (null after) . Exception.throwM $ InvalidAlias.new string
  src <- ModuleName.fromString before
  tgt <- Target.fromString . drop 1 $ after
  pure Alias {source = src, target = tgt}

toMap :: [Alias] -> Map.Map Target.Target Plugin.ModuleName
toMap = Map.fromListWith (const id) . fmap (\x -> (target x, source x))
