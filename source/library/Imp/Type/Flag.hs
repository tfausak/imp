module Imp.Type.Flag where

import qualified Control.Monad.Catch as Exception
import qualified Imp.Exception.InvalidOption as InvalidOption
import qualified Imp.Exception.UnexpectedArgument as UnexpectedArgument
import qualified Imp.Exception.UnknownOption as UnknownOption
import qualified System.Console.GetOpt as GetOpt

data Flag
  = Alias String
  | Help Bool
  | Package String
  | Version Bool
  deriving (Eq, Show)

options :: [GetOpt.OptDescr Flag]
options =
  [ GetOpt.Option
      ['h', '?']
      ["help"]
      (GetOpt.NoArg $ Help True)
      "Prints this help message then exits.",
    GetOpt.Option
      []
      ["no-help"]
      (GetOpt.NoArg $ Help False)
      "",
    GetOpt.Option
      ['v']
      ["version"]
      (GetOpt.NoArg $ Version True)
      "Prints the version number then exits.",
    GetOpt.Option
      []
      ["no-version"]
      (GetOpt.NoArg $ Version False)
      "",
    GetOpt.Option
      []
      ["alias"]
      (GetOpt.ReqArg Alias "SOURCE:TARGET")
      "Adds a new alias, allowing TARGET to be used in place of SOURCE. \
      \For example `--alias=Data.String:String` allows `String.words` to mean `Data.String.words`. \
      \Later aliases will overwrite earlier ones.",
    GetOpt.Option
      []
      ["package"]
      (GetOpt.ReqArg Package "MODULE:PACKAGE")
      "TODO"
  ]

fromArguments :: (Exception.MonadThrow m) => [String] -> m [Flag]
fromArguments arguments = do
  let (flgs, args, opts, errs) = GetOpt.getOpt' GetOpt.Permute options arguments
  mapM_ (Exception.throwM . UnexpectedArgument.new) args
  mapM_ (Exception.throwM . UnknownOption.new) opts
  mapM_ (Exception.throwM . InvalidOption.new) errs
  pure flgs
