{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}

module Imp where

import qualified Control.Monad as Monad
import qualified Control.Monad.Catch as Exception
import qualified Control.Monad.IO.Class as MonadIO
import qualified Data.Data as Data
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified GHC.Driver.Main as Driver
import qualified GHC.Hs as Hs
import qualified GHC.Plugins as Plugin
import qualified GHC.Types.Name.Cache as NameCache
import qualified Imp.Exception.ShowHelp as ShowHelp
import qualified Imp.Exception.ShowVersion as ShowVersion
import qualified Imp.Extra.Exception as Exception
import qualified Imp.Extra.HsModule as HsModule
import qualified Imp.Extra.HsParsedModule as HsParsedModule
import qualified Imp.Extra.ImportDecl as ImportDecl
import qualified Imp.Extra.Located as Located
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
parsedResultAction commandLineOptions _ parsedResult = do
  hscEnv <- Driver.getHscEnv
  Plugin.liftIO
    . Exception.handle handleException
    $ ParsedResult.overModule
      (HsParsedModule.overModule $ imp commandLineOptions (Plugin.hsc_units hscEnv) (Plugin.hsc_NC hscEnv))
      parsedResult

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
  (MonadIO.MonadIO m, Exception.MonadThrow m) =>
  [String] ->
  Plugin.UnitState ->
  NameCache.NameCache ->
  Plugin.Located Ghc.HsModulePs ->
  m (Plugin.Located Ghc.HsModulePs)
imp arguments unitState nameCache lHsModule = do
  flags <- Flag.fromArguments arguments
  config <- Config.fromFlags flags
  context <- Context.fromConfig config
  let aliases = Context.aliases context

  Located.overValue
    ( HsModule.overDecls $
        overData
          ( \x -> case Data.cast x of
              Nothing -> pure x
              Just old -> do
                new <- case old of
                  Plugin.Qual moduleName occName -> do
                    let target = Map.findWithDefault moduleName moduleName aliases
                    case Plugin.lookupModuleWithSuggestions unitState target Plugin.NoPkgQual of
                      Plugin.LookupFound module_ _ ->
                        fmap Plugin.Exact
                          . MonadIO.liftIO
                          . NameCache.updateNameCache nameCache module_ occName
                          $ \oldNameCache ->
                            case NameCache.lookupOrigNameCache oldNameCache module_ occName of
                              Just name -> pure (oldNameCache, name)
                              Nothing -> do
                                unique <- NameCache.takeUniqFromNameCache nameCache
                                let name = Plugin.mkExternalName unique module_ occName Plugin.generatedSrcSpan
                                    newNameCache = NameCache.extendOrigNameCache oldNameCache module_ occName name
                                pure (newNameCache, name)
                      _ -> pure old
                  _ -> pure old
                maybe (error "couldn't cast RdrName back into Data") pure $ Data.cast new
          )
    )
    lHsModule

overData :: (Data.Data a, Monad m) => (forall b. (Data.Data b) => b -> m b) -> a -> m a
overData g = Data.gmapM $ overData g Monad.>=> g

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
   in (Ghc.newImportDecl source)
        { Hs.ideclAs =
            if source == target
              then Nothing
              else Just $ Hs.noLocA target
        }
