{-# LANGUAGE TypeApplications #-}

module Imp where

import qualified Control.Monad.Catch as Exception
import qualified Data.Data as Data
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified GHC.Hs as Hs
import qualified GHC.Plugins as Plugin
import qualified Imp.Exception.ShowHelp as ShowHelp
import qualified Imp.Exception.ShowVersion as ShowVersion
import qualified Imp.Extra.Exception as Exception
import qualified Imp.Extra.HsModule as HsModule
import qualified Imp.Extra.HsParsedModule as HsParsedModule
import qualified Imp.Extra.ImportDecl as ImportDecl
import qualified Imp.Extra.ParsedResult as ParsedResult
import qualified Imp.Ghc as Ghc
import qualified Imp.Type.Config as Config
import qualified Imp.Type.Context as Context
import qualified Imp.Type.Flag as Flag
import qualified System.Exit as Exit
import qualified System.IO as IO

plugin :: Plugin.Plugin
plugin =
  Plugin.defaultPlugin
    { Plugin.parsedResultAction = parsedResultAction,
      Plugin.pluginRecompile = Plugin.purePlugin
    }

parsedResultAction ::
  [Plugin.CommandLineOption] ->
  modSummary ->
  Plugin.ParsedResult ->
  Plugin.Hsc Plugin.ParsedResult
parsedResultAction commandLineOptions _ parsedResult =
  Plugin.liftIO . Exception.handle handleException $ do
    flags <- Flag.fromArguments commandLineOptions
    config <- Config.fromFlags flags
    context <- Context.fromConfig config
    pure $ ParsedResult.overModule (HsParsedModule.overModule $ imp context) parsedResult

handleException :: Exception.SomeException -> IO a
handleException e = do
  IO.hPutStrLn IO.stderr $ Exception.displayException e
  Exit.exitWith $ exceptionToExitCode e

exceptionToExitCode :: Exception.SomeException -> Exit.ExitCode
exceptionToExitCode e
  | Exception.isType @ShowHelp.ShowHelp e = Exit.ExitSuccess
  | Exception.isType @ShowVersion.ShowVersion e = Exit.ExitSuccess
  | otherwise = Exit.ExitFailure 1

imp ::
  Context.Context ->
  Plugin.Located Ghc.HsModulePs ->
  Plugin.Located Ghc.HsModulePs
imp context lHsModule =
  let aliases = Context.aliases context
      moduleNames = Set.fromList $ biplate lHsModule :: Set.Set Plugin.ModuleName
   in fmap (HsModule.overImports $ updateImports aliases moduleNames) lHsModule

biplate :: (Data.Data a, Data.Data b) => a -> [b]
biplate =
  concat . Data.gmapQ (\d -> maybe (biplate d) pure $ Data.cast d)

updateImports ::
  Map.Map Plugin.ModuleName Plugin.ModuleName ->
  Set.Set Plugin.ModuleName ->
  [Hs.LImportDecl Hs.GhcPs] ->
  [Hs.LImportDecl Hs.GhcPs]
updateImports aliases want imports =
  let have = Set.fromList $ fmap (ImportDecl.toModuleName . Plugin.unLoc) imports
      need = Set.toList $ Set.difference want have
   in imports <> fmap (Hs.noLocA . createImport aliases) need

createImport ::
  Map.Map Plugin.ModuleName Plugin.ModuleName ->
  Plugin.ModuleName ->
  Hs.ImportDecl Hs.GhcPs
createImport aliases target =
  let source = Map.findWithDefault target target aliases
   in (Ghc.newImportDecl source) {Hs.ideclAs = Just $ Hs.noLocA target}
