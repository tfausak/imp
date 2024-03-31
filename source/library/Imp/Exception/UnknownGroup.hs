module Imp.Exception.UnknownGroup where

import qualified Control.Monad.Catch as Exception

newtype UnknownGroup
  = UnknownGroup String
  deriving (Eq, Show)

instance Exception.Exception UnknownGroup where
  displayException (UnknownGroup x) = "unknown group: " <> show x

new :: String -> UnknownGroup
new = UnknownGroup
