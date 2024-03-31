module Imp.Exception.InvalidPackageName where

import qualified Control.Monad.Catch as Exception

newtype InvalidPackageName
  = InvalidPackageName String
  deriving (Eq, Show)

instance Exception.Exception InvalidPackageName where
  displayException (InvalidPackageName x) = "invalid package name: " <> show x

new :: String -> InvalidPackageName
new = InvalidPackageName
