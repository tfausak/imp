{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Imp.Extra.Exception where

import qualified Control.Monad.Catch as Exception
import qualified Data.Maybe as Maybe

isType :: forall e. (Exception.Exception e) => Exception.SomeException -> Bool
isType = Maybe.isJust . Exception.fromException @e
