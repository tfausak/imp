module Imp.Extra.Either where

import qualified Control.Exception as Exception

throw :: (Exception.Exception e) => Either e a -> a
throw = either Exception.throw id
