module Imp.Type.Package where

import qualified Control.Monad as Monad
import qualified Control.Monad.Catch as Exception
import qualified Data.Map as Map
import qualified Imp.Exception.InvalidPackage as InvalidPackage
import qualified Imp.Type.PackageName as PackageName
import qualified Imp.Type.Target as Target

data Package = Package
  { module_ :: Target.Target,
    package :: PackageName.PackageName
  }
  deriving (Eq, Show)

fromString :: (Exception.MonadThrow m) => String -> m Package
fromString string = do
  let (before, after) = break (== ':') string
  Monad.when (null after) . Exception.throwM $ InvalidPackage.new string
  mdl <- Target.fromString before
  pkg <- PackageName.fromString . drop 1 $ after
  pure Package {module_ = mdl, package = pkg}

toMap :: [Package] -> Map.Map Target.Target PackageName.PackageName
toMap = Map.fromListWith (const id) . fmap (\x -> (module_ x, package x))
