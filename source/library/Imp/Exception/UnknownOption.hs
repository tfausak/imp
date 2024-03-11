module Imp.Exception.UnknownOption where

import qualified Control.Monad.Catch as Exception

newtype UnknownOption
  = UnknownOption String
  deriving (Eq, Show)

instance Exception.Exception UnknownOption where
  displayException (UnknownOption x) = "unknown option: " <> show x

new :: String -> UnknownOption
new = UnknownOption
