module Imp.Type.PackageName where

import qualified GHC.Plugins as Plugin

-- TODO: Use Distribution.Types.PackageName from Cabal-syntax.
newtype PackageName
  = PackageName Plugin.PackageName
  deriving (Eq)

instance Show PackageName where
  show = const "TODO"

fromPackageName :: Plugin.PackageName -> PackageName
fromPackageName = PackageName

toPackageName :: PackageName -> Plugin.PackageName
toPackageName (PackageName x) = x

fromString :: (Applicative m) => String -> m PackageName
fromString = pure . fromPackageName . Plugin.PackageName . Plugin.mkFastString
