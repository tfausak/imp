module Imp.Exception.InvalidPackage where

import qualified Control.Monad.Catch as Exception

newtype InvalidPackage
  = InvalidPackage String
  deriving (Eq, Show)

instance Exception.Exception InvalidPackage where
  displayException (InvalidPackage x) = "invalid package: " <> show x

new :: String -> InvalidPackage
new = InvalidPackage
