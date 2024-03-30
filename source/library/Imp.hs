{-# LANGUAGE TypeApplications #-}

module Imp where

import qualified Control.Monad.Catch as Exception
import qualified Data.Data as Data
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
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
import qualified Imp.Extra.SrcSpanAnnN as SrcSpanAnnN
import qualified Imp.Ghc as Ghc
import qualified Imp.Type.Config as Config
import qualified Imp.Type.Context as Context
import qualified Imp.Type.Flag as Flag
import qualified Imp.Type.Target as Target
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
parsedResultAction commandLineOptions _ =
  Plugin.liftIO
    . Exception.handle handleException
    . ParsedResult.overModule (HsParsedModule.overModule $ imp commandLineOptions)

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
  (Exception.MonadThrow m) =>
  [String] ->
  Plugin.Located Ghc.HsModulePs ->
  m (Plugin.Located Ghc.HsModulePs)
imp arguments lHsModule = do
  flags <- Flag.fromArguments arguments
  config <- Config.fromFlags flags
  context <- Context.fromConfig config
  let aliases = Context.aliases context
      moduleNames =
        Map.fromListWith SrcSpanAnnN.leftmostSmallest
          . Maybe.mapMaybe
            ( \lRdrName -> case Plugin.unLoc lRdrName of
                Plugin.Qual moduleName _ -> Just (moduleName, Plugin.getLoc lRdrName)
                _ -> Nothing
            )
          . biplate
          . Hs.hsmodDecls
          $ Plugin.unLoc lHsModule
  pure $ fmap (HsModule.overImports $ updateImports aliases moduleNames) lHsModule

biplate :: (Data.Data a, Data.Data b) => a -> [b]
biplate = concat . Data.gmapQ (\d -> maybe (biplate d) pure $ Data.cast d)

updateImports ::
  Map.Map Target.Target Plugin.ModuleName ->
  Map.Map Plugin.ModuleName Hs.SrcSpanAnnN ->
  [Hs.LImportDecl Hs.GhcPs] ->
  [Hs.LImportDecl Hs.GhcPs]
updateImports aliases want imports =
  let have = Set.fromList $ fmap (ImportDecl.toModuleName . Plugin.unLoc) imports
      need = Map.toList $ Map.withoutKeys want have
   in imports <> fmap (\(m, l) -> Plugin.L (Hs.na2la l) $ createImport aliases m) need

createImport ::
  Map.Map Target.Target Plugin.ModuleName ->
  Plugin.ModuleName ->
  Hs.ImportDecl Hs.GhcPs
createImport aliases target =
  let source = Map.findWithDefault target (Target.fromModuleName target) aliases
   in (Ghc.newImportDecl source)
        { Hs.ideclAs =
            if source == target
              then Nothing
              else Just $ Hs.noLocA target
        }
