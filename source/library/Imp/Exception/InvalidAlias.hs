module Imp.Exception.InvalidAlias where

import qualified Control.Monad.Catch as Exception

newtype InvalidAlias
  = InvalidAlias String
  deriving (Eq, Show)

instance Exception.Exception InvalidAlias where
  displayException (InvalidAlias x) = "invalid alias: " <> show x

new :: String -> InvalidAlias
new = InvalidAlias
