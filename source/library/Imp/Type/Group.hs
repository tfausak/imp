module Imp.Type.Group where

import qualified Control.Monad.Catch as Exception
import qualified Imp.Exception.UnknownGroup as UnknownGroup
import qualified Imp.Extra.Either as Either
import qualified Imp.Type.Alias as Alias
import qualified Imp.Type.Source as Source
import qualified Imp.Type.Target as Target

data Group
  = Base
  -- TODO: Add more groups.
  deriving (Eq, Show)

fromString :: (Exception.MonadThrow m) => String -> m Group
fromString x = case x of
  "base" -> pure Base
  _ -> Exception.throwM $ UnknownGroup.new x

toAliases :: Group -> [Alias.Alias]
toAliases x =
  let alias s t =
        Alias.Alias
          { Alias.source = Either.throw $ Source.fromString s,
            Alias.target = Either.throw $ Target.fromString t
          }
   in case x of
        Base ->
          [ alias "Control.Applicative" "Applicative",
            alias "Control.Arrow" "Arrow",
            alias "Control.Category" "Category",
            alias "Control.Concurrent" "Concurrent",
            alias "Control.Exception" "Exception",
            alias "Control.Monad" "Monad",
            alias "Control.Monad.IO.Class" "MonadIO"
            -- TODO: Add more aliases.
          ]
