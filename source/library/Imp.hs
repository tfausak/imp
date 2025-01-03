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
import qualified GHC.Types.PkgQual as PkgQual
import qualified GHC.Types.SourceText as SourceText
import qualified Imp.Exception.ShowHelp as ShowHelp
import qualified Imp.Exception.ShowVersion as ShowVersion
import qualified Imp.Extra.Exception as Exception
import qualified Imp.Extra.HsModule as HsModule
import qualified Imp.Extra.HsParsedModule as HsParsedModule
import qualified Imp.Extra.ImportDecl as ImportDecl
import qualified Imp.Extra.ParsedResult as ParsedResult
import qualified Imp.Extra.SrcSpanAnnN as SrcSpanAnnN
import qualified Imp.Type.Config as Config
import qualified Imp.Type.Context as Context
import qualified Imp.Type.Flag as Flag
import qualified Imp.Type.PackageName as PackageName
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
  Plugin.Located (Hs.HsModule Hs.GhcPs) ->
  m (Plugin.Located (Hs.HsModule Hs.GhcPs))
imp arguments this lHsModule = do
  flags <- Flag.fromArguments arguments
  config <- Config.fromFlags flags
  context <- Context.fromConfig config
  let aliases = Context.aliases context
      packages = Context.packages context
      implicits =
        Set.map Target.toModuleName
          . Map.keysSet
          $ Map.filter Source.isImplicit aliases
      -- I would prefer to use `hsmodImports`, but I get a spurious warning
      -- with GHC 9.10.1.
      -- <https://github.com/tfausak/imp/pull/24#issuecomment-2116480980>
      imports = case Plugin.unLoc lHsModule of
        Hs.HsModule _ _ _ lImportDecls _ ->
          Set.fromList $
            fmap (ImportDecl.toModuleName . Plugin.unLoc) lImportDecls
      go ::
        (Data.Data a) =>
        a ->
        StateT.State (Map.Map Plugin.ModuleName Hs.SrcSpanAnnN) a
      go = overData $ updateQualifiedIdentifiers this implicits imports
      (newLHsModule, moduleNames) =
        StateT.runState
          (go lHsModule)
          Map.empty
  pure $ fmap (HsModule.overImports $ updateImports this aliases packages moduleNames) newLHsModule

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
  Map.Map Target.Target PackageName.PackageName ->
  Map.Map Plugin.ModuleName Hs.SrcSpanAnnN ->
  [Hs.LImportDecl Hs.GhcPs] ->
  [Hs.LImportDecl Hs.GhcPs]
updateImports this aliases packages want imports =
  let have = Set.insert this . Set.fromList $ fmap (ImportDecl.toModuleName . Plugin.unLoc) imports
      need = Map.toList $ Map.withoutKeys want have
   in imports <> Maybe.mapMaybe (\(m, l) -> Plugin.L (Hs.l2l l) <$> createImport aliases packages m) need

createImport ::
  Map.Map Target.Target Source.Source ->
  Map.Map Target.Target PackageName.PackageName ->
  Plugin.ModuleName ->
  Maybe (Hs.ImportDecl Hs.GhcPs)
createImport aliases packages target = do
  source <-
    case Map.lookup (Target.fromModuleName target) aliases of
      Nothing -> Just target
      Just s -> case s of
        Source.Implicit -> Nothing
        Source.Explicit m -> Just m
  Just
    Hs.ImportDecl
      { Hs.ideclExt =
          Hs.XImportDeclPass
            { Hs.ideclAnn = Hs.noAnn,
              Hs.ideclSourceText = SourceText.NoSourceText,
              Hs.ideclImplicit = True
            },
        Hs.ideclName = Hs.noLocA source,
        Hs.ideclPkgQual =
          maybe PkgQual.NoRawPkgQual (PkgQual.RawPkgQual . PackageName.toStringLiteral) $
            Map.lookup (Target.fromModuleName target) packages,
        Hs.ideclSource = Hs.NotBoot,
        Hs.ideclSafe = False,
        Hs.ideclQualified = Hs.QualifiedPre,
        Hs.ideclAs =
          if source == target
            then Nothing
            else Just $ Hs.noLocA target,
        Hs.ideclImportList = Nothing
      }
