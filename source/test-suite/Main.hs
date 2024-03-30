{-# LANGUAGE CPP #-}

import qualified Control.Monad.Catch as Exception
import qualified GHC.Data.EnumSet as EnumSet
import qualified GHC.Data.FastString as FastString
import qualified GHC.Data.StringBuffer as StringBuffer
import qualified GHC.Parser as Parser
import qualified GHC.Parser.Lexer as Lexer
import qualified GHC.Stack as Stack
import qualified GHC.Types.SrcLoc as SrcLoc
import qualified GHC.Utils.Error as Error
import qualified GHC.Utils.Outputable as Outputable
import qualified Imp
import qualified Imp.Ghc as Ghc
import qualified Language.Haskell.Syntax.Module.Name as ModuleName
import qualified Test.Hspec as Hspec

main :: IO ()
main = Hspec.hspec . Hspec.parallel . Hspec.describe "Imp" $ do
  Hspec.it "does nothing with an empty module" $ do
    expectImp
      []
      ""
      ""

  Hspec.it "does nothing when nothing needs to be imported" $ do
    expectImp
      []
      "true = True"
      "true = True"

  Hspec.it "inserts an import for a qualified value" $ do
    expectImp
      []
      "true = Data.Bool.True"
      "import (implicit) qualified Data.Bool\ntrue = Data.Bool.True"

  Hspec.it "inserts an aliased import" $ do
    expectImp
      ["--alias=Data.Bool:Bool"]
      "true = Bool.True"
      "import (implicit) qualified Data.Bool as Bool\ntrue = Bool.True"

  Hspec.it "prefers later aliases over earlier ones" $ do
    expectImp
      ["--alias=Relude.Bool:Bool", "--alias=Data.Bool:Bool"]
      "true = Bool.True"
      "import (implicit) qualified Data.Bool as Bool\ntrue = Bool.True"

  Hspec.it "inserts an import for a qualified type" $ do
    expectImp
      []
      "true = True :: Data.Bool.Bool"
      "import (implicit) qualified Data.Bool\ntrue = True :: Data.Bool.Bool"

  Hspec.it "inserts multiple imports sorted" $ do
    expectImp
      []
      "true :: Relude.Bool.Bool\ntrue = Data.Bool.True"
      "import (implicit) qualified Data.Bool\nimport (implicit) qualified Relude.Bool\ntrue :: Relude.Bool.Bool\ntrue = Data.Bool.True"

  Hspec.it "does not re-import an open import" $ do
    expectImp
      []
      "import Data.Bool\ntrue = Data.Bool.True"
      "import Data.Bool\ntrue = Data.Bool.True"

  Hspec.it "does not re-import a qualified import" $ do
    expectImp
      []
      "import qualified Data.Bool\ntrue = Data.Bool.True"
      "import qualified Data.Bool\ntrue = Data.Bool.True"

  Hspec.it "does not re-import an aliased import" $ do
    expectImp
      []
      "import qualified Data.Bool as Bool\ntrue = Bool.True"
      "import qualified Data.Bool as Bool\ntrue = Bool.True"

  Hspec.it "inserts imports after existing ones" $ do
    expectImp
      []
      "import qualified Relude.Bool\ntrue :: Relude.Bool.Bool\ntrue = Data.Bool.True"
      "import qualified Relude.Bool\nimport (implicit) qualified Data.Bool\ntrue :: Relude.Bool.Bool\ntrue = Data.Bool.True"

  Hspec.it "replaces implicit source with current module" $ do
    expectImp
      ["--alias=_:This"]
      "undefined = This.undefined"
      "undefined = Example.undefined"

  Hspec.it "does not clobber import with implicit" $ do
    expectImp
      ["--alias=_:Data.Bool"]
      "import Data.Bool\ntrue = Data.Bool.True"
      "import Data.Bool\ntrue = Data.Bool.True"

expectImp :: (Stack.HasCallStack) => [String] -> String -> String -> Hspec.Expectation
expectImp arguments input expected = do
  before <- parseModule input
  after <- Imp.imp arguments (ModuleName.mkModuleName "Example") before
  let actual = Outputable.showPprUnsafe after
  actual `Hspec.shouldBe` expected

parseModule :: (Exception.MonadThrow m) => String -> m (SrcLoc.Located Ghc.HsModulePs)
parseModule input = do
  let parserOpts = Lexer.mkParserOpts EnumSet.empty emptyDiagOpts [] False False False False
      stringBuffer = StringBuffer.stringToStringBuffer input
      realSrcLoc = SrcLoc.mkRealSrcLoc (FastString.mkFastString "<interactive>") 1 1
      pState = Lexer.initParserState parserOpts stringBuffer realSrcLoc
      parseResult = Lexer.unP Parser.parseModule pState
  case parseResult of
    Lexer.PFailed _ -> Exception.throwM $ InvalidInput input
    Lexer.POk _ lHsModule -> pure lHsModule

emptyDiagOpts :: Error.DiagOpts
#if MIN_VERSION_ghc(9, 8, 1)
emptyDiagOpts = Error.emptyDiagOpts
#else
emptyDiagOpts = Error.DiagOpts EnumSet.empty EnumSet.empty False False Nothing Outputable.defaultSDocContext
#endif

newtype InvalidInput
  = InvalidInput String
  deriving (Eq, Show)

instance Exception.Exception InvalidInput
