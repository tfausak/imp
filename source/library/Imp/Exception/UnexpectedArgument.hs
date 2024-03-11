module Imp.Exception.UnexpectedArgument where

import qualified Control.Monad.Catch as Exception

newtype UnexpectedArgument
  = UnexpectedArgument String
  deriving (Eq, Show)

instance Exception.Exception UnexpectedArgument where
  displayException (UnexpectedArgument x) = "unexpected argument: " <> show x

new :: String -> UnexpectedArgument
new = UnexpectedArgument
