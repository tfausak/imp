module Imp.Type.Alias where

import qualified Control.Monad as Monad
import qualified Control.Monad.Catch as Exception
import qualified Data.Map as Map
import qualified Imp.Exception.InvalidAlias as InvalidAlias
import qualified Imp.Type.Source as Source
import qualified Imp.Type.Target as Target

data Alias = Alias
  { source :: Source.Source,
    target :: Target.Target
  }
  deriving (Eq, Show)

fromString :: (Exception.MonadThrow m) => String -> m Alias
fromString string = do
  let (before, after) = break (== ':') string
  Monad.when (null after) . Exception.throwM $ InvalidAlias.new string
  src <- Source.fromString before
  tgt <- Target.fromString . drop 1 $ after
  pure Alias {source = src, target = tgt}

toMap :: [Alias] -> Map.Map Target.Target Source.Source
toMap = Map.fromListWith (const id) . fmap (\x -> (target x, source x))
