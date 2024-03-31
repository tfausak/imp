module Imp.Type.PackageName where

import qualified Control.Monad.Catch as Exception
import qualified Distribution.Parsec as Parsec
import qualified Distribution.Types.PackageName as PackageName
import qualified GHC.Data.FastString as FastString
import qualified GHC.Types.SourceText as SourceText
import qualified Imp.Exception.InvalidPackageName as InvalidPackageName

newtype PackageName
  = PackageName PackageName.PackageName
  deriving (Eq, Show)

fromCabal :: PackageName.PackageName -> PackageName
fromCabal = PackageName

toCabal :: PackageName -> PackageName.PackageName
toCabal (PackageName x) = x

fromString :: (Exception.MonadThrow m) => String -> m PackageName
fromString x =
  maybe (Exception.throwM $ InvalidPackageName.new x) (pure . fromCabal) $
    Parsec.simpleParsec x

toStringLiteral :: PackageName -> SourceText.StringLiteral
toStringLiteral x =
  SourceText.StringLiteral
    { SourceText.sl_st = SourceText.NoSourceText,
      SourceText.sl_fs = FastString.mkFastString . PackageName.unPackageName $ toCabal x,
      SourceText.sl_tc = Nothing
    }
