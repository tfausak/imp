{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}

module Imp where

import qualified Control.Monad as Monad
import qualified Control.Monad.Catch as Exception
import qualified Control.Monad.Trans.State as StateT
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
import qualified Imp.Extra.Located as Located
import qualified Imp.Extra.ParsedResult as ParsedResult
import qualified Imp.Extra.SrcSpanAnnN as SrcSpanAnnN
import qualified Imp.Ghc as Ghc
import qualified Imp.Type.Config as Config
import qualified Imp.Type.Context as Context
import qualified Imp.Type.Flag as Flag
import qualified Imp.Type.Source as Source
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
  Plugin.ModSummary ->
  Plugin.ParsedResult ->
  Plugin.Hsc Plugin.ParsedResult
parsedResultAction commandLineOptions modSummary =
  Plugin.liftIO
    . Exception.handle handleException
    . ParsedResult.overModule
      ( HsParsedModule.overModule
          . imp commandLineOptions
          $ Plugin.ms_mod_name modSummary
      )

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
  Plugin.ModuleName ->
  Plugin.Located Ghc.HsModulePs ->
  m (Plugin.Located Ghc.HsModulePs)
imp arguments this lHsModule = do
  flags <- Flag.fromArguments arguments
  config <- Config.fromFlags flags
  context <- Context.fromConfig config
  let aliases = Context.aliases context
      implicits =
        Set.map Target.toModuleName
          . Map.keysSet
          $ Map.filter Source.isImplicit aliases
      imports =
        Set.fromList
          . fmap (ImportDecl.toModuleName . Plugin.unLoc)
          . Hs.hsmodImports
          $ Plugin.unLoc lHsModule
      (newLHsModule, moduleNames) =
        StateT.runState
          (Located.overValue (HsModule.overDecls $ overData $ updateQualifiedIdentifiers this implicits imports) lHsModule)
          Map.empty
  pure $ fmap (HsModule.overImports $ updateImports this aliases moduleNames) newLHsModule

updateQualifiedIdentifiers ::
  (Data.Data a) =>
  Plugin.ModuleName ->
  Set.Set Plugin.ModuleName ->
  Set.Set Plugin.ModuleName ->
  a ->
  StateT.State (Map.Map Plugin.ModuleName Hs.SrcSpanAnnN) a
updateQualifiedIdentifiers this implicits imports x = case Data.cast x of
  Nothing -> pure x
  Just (Plugin.L l rdrName) -> case rdrName of
    Plugin.Qual moduleName occName ->
      if Set.notMember moduleName imports && Set.member moduleName implicits
        then
          pure
            . Maybe.fromMaybe x
            . Data.cast
            . Plugin.L l
            $ Plugin.Qual this occName
        else do
          StateT.modify $
            Map.insertWith
              SrcSpanAnnN.leftmostSmallest
              moduleName
              l
          pure x
    _ -> pure x

overData :: (Data.Data a, Monad m) => (forall b. (Data.Data b) => b -> m b) -> a -> m a
overData f = Data.gmapM $ overData f Monad.>=> f

updateImports ::
  Plugin.ModuleName ->
  Map.Map Target.Target Source.Source ->
  Map.Map Plugin.ModuleName Hs.SrcSpanAnnN ->
  [Hs.LImportDecl Hs.GhcPs] ->
  [Hs.LImportDecl Hs.GhcPs]
updateImports this aliases want imports =
  let have = Set.insert this . Set.fromList $ fmap (ImportDecl.toModuleName . Plugin.unLoc) imports
      need = Map.toList $ Map.withoutKeys want have
   in imports <> Maybe.mapMaybe (\(m, l) -> Plugin.L (Hs.na2la l) <$> createImport aliases m) need

createImport ::
  Map.Map Target.Target Source.Source ->
  Plugin.ModuleName ->
  Maybe (Hs.ImportDecl Hs.GhcPs)
createImport aliases target = do
  source <-
    case Map.lookup (Target.fromModuleName target) aliases of
      Nothing -> Just target
      Just s -> case s of
        Source.Implicit -> Nothing
        Source.Explicit m -> Just m
  Just
    (Ghc.newImportDecl source)
      { Hs.ideclAs =
          if source == target
            then Nothing
            else Just $ Hs.noLocA target
      }
