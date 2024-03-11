module Imp.Exception.InvalidModuleName where

import qualified Control.Monad.Catch as Exception

newtype InvalidModuleName
  = InvalidModuleName String
  deriving (Eq, Show)

instance Exception.Exception InvalidModuleName where
  displayException (InvalidModuleName x) = "invalid module name: " <> show x

new :: String -> InvalidModuleName
new = InvalidModuleName
