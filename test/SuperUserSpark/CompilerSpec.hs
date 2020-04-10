{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TemplateHaskell #-}

module SuperUserSpark.CompilerSpec where

import TestImport hiding ((<.>))

import Data.Either (isLeft, isRight)
import Data.List (isPrefixOf)
import System.FilePath.Posix ((<.>))

import SuperUserSpark.Compiler
import SuperUserSpark.Compiler.Gen ()
import SuperUserSpark.Compiler.Internal
import SuperUserSpark.Compiler.TestUtils
import SuperUserSpark.Compiler.Types
import SuperUserSpark.Compiler.Utils
import SuperUserSpark.CoreTypes
import SuperUserSpark.Language.Gen ()
import SuperUserSpark.Language.Types
import SuperUserSpark.OptParse.Gen ()
import SuperUserSpark.PreCompiler
import SuperUserSpark.Utils
import TestUtils

spec :: Spec
spec = do
    parallel $ do
        instanceSpec
        singleCompileDecSpec
        precompileSpec
        compileUnitSpec
        utilsSpec
    hopTests
    exactTests
    compilerBlackBoxTests

precompileSpec :: Spec
precompileSpec = describe "pre-compilation" $ do cleanContentSpec

cleanContentSpec :: Spec
cleanContentSpec = do
    let validFp = genValid `suchThat` cleanBy cleanFilePath
    describe "cleanCard" $ do
        it "doesn't report any card with valid content and a valid name" $ do
            forAll (genValid `suchThat` cleanBy cleanCardName) $ \cn ->
                forAll (genValid `suchThat` cleanBy cleanDeclaration) $ \cc ->
                    Card cn cc `shouldSatisfy` cleanBy cleanCard
    describe "cleanCardName" $ do
        pend
        it "doesn't report an emty card name" $ do
            "" `shouldSatisfy` cleanBy cleanCardName
        it "reports card names with newlines" $ do
            forAll (sequenceA [genValid, pure '\n', genValid]) $ \s ->
                s `shouldNotSatisfy` cleanBy cleanCardName
    describe "cleanDeclaration" $ do
        describe "Deploy" $ do
            it "doesn't report Deploy declarations with valid filepaths" $ do
                forAll validFp $ \src ->
                    forAll validFp $ \dst ->
                        forAll genValid $ \kind ->
                            Deploy src dst kind `shouldSatisfy`
                            cleanBy cleanDeclaration
            it "reports Deploy declarations with an invalid source" $ do
                forAll (genValid `suchThat` (not . cleanBy cleanFilePath)) $ \src ->
                    forAll validFp $ \dst ->
                        forAll genValid $ \kind ->
                            Deploy src dst kind `shouldNotSatisfy`
                            cleanBy cleanDeclaration
            it "reports Deploy declarations with an invalid destination" $ do
                forAll validFp $ \src ->
                    forAll (genValid `suchThat` (not . cleanBy cleanFilePath)) $ \dst ->
                        forAll genValid $ \kind ->
                            Deploy src dst kind `shouldNotSatisfy`
                            cleanBy cleanDeclaration
            pend
        describe "SparkOff" $ do
            it "reports SparkOff declarations with an invalid card reference" $ do
                forAll (genValid `suchThat` (not . cleanBy cleanCardReference)) $ \cr ->
                    SparkOff cr `shouldNotSatisfy` cleanBy cleanDeclaration
            it
                "doesn't report SparkOff declarations with a valid card reference" $ do
                forAll (genValid `suchThat` cleanBy cleanCardReference) $ \cr ->
                    SparkOff cr `shouldSatisfy` cleanBy cleanDeclaration
            pend
        describe "IntoDir" $ do
            it "reports IntoDir declarations with an invalid filepath" $ do
                forAll (genValid `suchThat` (not . cleanBy cleanFilePath)) $ \fp ->
                    IntoDir fp `shouldNotSatisfy` cleanBy cleanDeclaration
            it "doesn't report IntoDir declarations with a valid filepath" $ do
                forAll (genValid `suchThat` cleanBy cleanFilePath) $ \fp ->
                    IntoDir fp `shouldSatisfy` cleanBy cleanDeclaration
            pend
        describe "OutofDir" $ do
            it "reports OutofDir declarations with an invalid filepath" $ do
                forAll (genValid `suchThat` (not . cleanBy cleanFilePath)) $ \fp ->
                    OutofDir fp `shouldNotSatisfy` cleanBy cleanDeclaration
            it "doesn't report OutofDir declarations with a valid filepath" $ do
                forAll (genValid `suchThat` cleanBy cleanFilePath) $ \fp ->
                    OutofDir fp `shouldSatisfy` cleanBy cleanDeclaration
            pend
        describe "DeployKindOverride" $ do
            it "doesn't report any deployment kind override declarations" $ do
                forAll genValid $ \kind ->
                    DeployKindOverride kind `shouldSatisfy`
                    cleanBy cleanDeclaration
            pend
        describe "Alternatives" $ do
            it
                "reports alternatives declarations with as much as a single invalid filepath" $ do
                forAll (genValid `suchThat` (any $ not . cleanBy cleanFilePath)) $ \fs ->
                    Alternatives fs `shouldNotSatisfy` cleanBy cleanDeclaration
            it "doesn't report alternatives declarations with valid filepaths" $ do
                forAll (genValid `suchThat` (all $ cleanBy cleanFilePath)) $ \fs ->
                    Alternatives fs `shouldSatisfy` cleanBy cleanDeclaration
            pend
        describe "Block" $ do
            it
                "reports block declarations with as much as a single invalid declaration inside" $ do
                forAll
                    (genValid `suchThat` (any $ not . cleanBy cleanDeclaration)) $ \ds ->
                    Block ds `shouldNotSatisfy` cleanBy cleanDeclaration
            it
                "doesn't report any block declarations with valid declarations inside" $ do
                forAll (genValid `suchThat` (all $ cleanBy cleanDeclaration)) $ \ds ->
                    Block ds `shouldSatisfy` cleanBy cleanDeclaration
            pend
    describe "cleanCardReference" $ do
        it "works the same as cleanCardName separately" $ do
            forAll genValid $ \cnr ->
                cleanBy cleanCardNameReference cnr ===
                cleanBy cleanCardReference (CardName cnr)
        it "works the same as cleanCardFile separately" $ do
            forAll genValid $ \cfr ->
                cleanBy cleanCardFileReference cfr ===
                cleanBy cleanCardReference (CardFile cfr)
        pend
    describe "cleanCardNameReference" $ do
        it "reports card name references with an invalid card name" $ do
            forAll (genValid `suchThat` (not . cleanBy cleanCardName)) $ \cn ->
                CardNameReference cn `shouldNotSatisfy`
                cleanBy cleanCardNameReference
        it "doesn't report card name references with a valid card name" $ do
            forAll (genValid `suchThat` cleanBy cleanCardName) $ \cn ->
                CardNameReference cn `shouldSatisfy`
                cleanBy cleanCardNameReference
        pend
    describe "cleanCardFileReference" $ do
        it "reports card file references with an invalid filepath" $ do
            forAll (genValid `suchThat` (not . cleanBy cleanFilePath)) $ \fp ->
                forAll genValid $ \cn ->
                    CardFileReference fp cn `shouldNotSatisfy`
                    cleanBy cleanCardFileReference
        it "reports card file references with an invalid card name" $ do
            forAll genValid $ \fp ->
                forAll
                    (genValid `suchThat` (not . cleanBy cleanCardNameReference)) $ \cn ->
                    CardFileReference fp (Just cn) `shouldNotSatisfy`
                    cleanBy cleanCardFileReference
        it
            "doesn't report card file references with a valid card name reference and valid filepath" $ do
            forAll (genValid `suchThat` cleanBy cleanFilePath) $ \fp ->
                forAll (genValid `suchThat` cleanBy cleanCardNameReference) $ \cn ->
                    CardFileReference fp (Just cn) `shouldSatisfy`
                    cleanBy cleanCardFileReference
        pend
    describe "cleanFilePath" $ do
        it "reports empty an filepath" $ do filePathDirty []
        let nonNull = genValid `suchThat` (not . null)
        it "reports filepaths with newlines" $ do
            forAll (nonNull `suchThat` containsNewlineCharacter) filePathDirty
        let withoutNewlines =
                nonNull `suchThat` (not . containsNewlineCharacter)
        it "reports filepaths with multiple consequtive slashes" $ do
            once $
                forAll
                    (withoutNewlines `suchThat`
                     containsMultipleConsequtiveSlashes)
                    filePathDirty
        let c = filePathClean
        it "doesn't report these valid filepaths" $ do
            c "noextension"
            c ".bashrc"
            c "file.txt"
            c "Some file with spaces.doc"
            c "some/relative/filepath.file"

-- TODO(syd) Use the default config to generate this!
defaultCompilerState :: CompilerState
defaultCompilerState =
    CompilerState
    { stateDeploymentKindLocalOverride = Nothing
    , stateInto = ""
    , stateOutofPrefix = []
    }

instanceSpec :: Spec
instanceSpec =
    parallel $ do
        eqSpec @CompileAssignment
        genValidSpec @CompileAssignment
        eqSpec @CompileSettings
        genValidSpec @CompileSettings
        eqSpec @(Deployment FilePath)
        genValidSpec @(Deployment FilePath)
        jsonSpecOnValid @(Deployment FilePath)
        functorSpec @Deployment
        eqSpec @(DeploymentDirections FilePath)
        genValidSpec @(DeploymentDirections FilePath)
        jsonSpecOnValid @(DeploymentDirections FilePath)
        functorSpec @DeploymentDirections
        eqSpec @PrefixPart
        genValidSpec @PrefixPart
        eqSpec @CompilerState
        genValidSpec @CompilerState


singleCompileDecSpec :: Spec
singleCompileDecSpec =
    describe "compileDec" $ do
        let s = defaultCompilerState
        let c = defaultCompileSettings
        let sc = singleShouldCompileTo c s
        let nonNull = genValid `suchThat` (not . null)
        let validFilePath = nonNull `suchThat` (not . containsNewlineCharacter)
        let easyFilePath = validFilePath `suchThat` (not . isPrefixOf ".")
        let validFp = genValid `suchThat` cleanBy cleanFilePath
        describe "Deploy" $ do
            it
                "uses the exact right text in source and destination when given valid filepaths without a leading dot" $ do
                forAll easyFilePath $ \from ->
                    forAll easyFilePath $ \to ->
                        sc
                            (Deploy from to Nothing)
                            (Deployment (Directions [from] to) LinkDeployment)
            it "handles filepaths with a leading dot correctly" $ do pending
            it
                "figures out the correct paths in these cases with default config and initial state" $ do
                let d = (Deploy "from" "to" $ Just LinkDeployment)
                sc d (Deployment (Directions ["from"] "to") LinkDeployment)
            it "uses the alternates correctly" $ do pending
            it "uses the into's correctly" $ do pending
            it "uses the outof's correctly" $ do pending
            pend
        describe "SparkOff" $ do
            it
                "adds a single card file reference to the list of cards to spark later" $ do
                forAll validFilePath $ \f ->
                    let cr = CardFile $ CardFileReference f Nothing
                        d = SparkOff cr
                    in compileSingleDec d s c `shouldBe` Right (s, ([], [cr]))
            it "adds any card reference to the list" $ do pending
            pend
        let shouldState = shouldResultInState c s
        describe "IntoDir" $ do
            it "adds the given directory to the into state" $ do
                forAll validFp $ \fp ->
                    shouldState (IntoDir fp) $ s {stateInto = fp}
            it "compounds with the input state" $ do
                pendingWith "Change the input state to an explicit list first"
            pend
        describe "OutofDir" $ do
            it "adds the given directory to the outof state" $ do
                forAll validFp $ \fp ->
                    shouldState (OutofDir fp) $
                    s {stateOutofPrefix = [Literal fp]}
            pend
        describe "DeployKindOverride" $ do
            it "modifies the internal deployment kind override" $ do
                forAll genValid $ \kind ->
                    shouldState (DeployKindOverride kind) $
                    s {stateDeploymentKindLocalOverride = Just kind}
            pend
        describe "Block" $ do
            it "uses a separate scope for its sub-compilation" $ do
                forAll genValid $ \ds -> shouldState (Block ds) s
            pend
        describe "Alternatives" $ do
            it
                "adds an alternatives prefix to the outof prefix in the compiler state" $ do
                forAll (listOf validFilePath) $ \fps ->
                    shouldState (Alternatives fps) $
                    s {stateOutofPrefix = [Alts fps]}
            pend

runDefaultImpureCompiler :: ImpureCompiler a -> IO (Either CompileError a)
runDefaultImpureCompiler = flip runReaderT defaultCompileSettings . runExceptT

compileUnitSpec :: Spec
compileUnitSpec =
    describe "compileUnit" $ do
        it "Only ever produces valid results" $
            forAll genValid $ \sets ->
                validIfSucceeds
                    (runIdentity .
                     flip runReaderT sets . runExceptT . compileUnit)

utilsSpec :: Spec
utilsSpec =
    parallel $ do
        describe "initialState" $ it "is valid" $ isValid initialState
        describe "sources" $
            it "only produces valid prefix parts" $ producesValid sources
        describe "resolvePrefix" $
            it "only produces valid paths" $ producesValid resolvePrefix

hopTests :: Spec
hopTests = do
    describe "hop test" $ do
        dir <- runIO $ resolveDir' "test_resources/hop_test"
        let root = dir </> $(mkRelFile "root.sus")
        let hop1 = dir </> $(mkRelDir "hop1dir") </> $(mkRelFile "hop1.sus")
        let hop2 =
                dir </> $(mkRelDir "hop1dir") </> $(mkRelDir "hop2dir") </>
                $(mkRelFile "hop2.sus")
        let hop3 =
                dir </> $(mkRelDir "hop1dir") </> $(mkRelDir "hop2dir") </>
                $(mkRelDir "hop3dir") </>
                $(mkRelFile "hop3.sus")
        it "compiles hop3 correctly" $ do
            r <-
                runDefaultImpureCompiler $
                compileJob $ StrongCardFileReference hop3 Nothing
            r `shouldBe`
                Right
                    [ Deployment
                          (Directions ["z/delta"] "d/three")
                          LinkDeployment
                    ]
        it "compiles hop2 correctly" $ do
            r <-
                runDefaultImpureCompiler $
                compileJob $ StrongCardFileReference hop2 Nothing
            r `shouldBe`
                Right
                    [ Deployment (Directions ["y/gamma"] "c/two") LinkDeployment
                    , Deployment
                          (Directions ["hop3dir/z/delta"] "d/three")
                          LinkDeployment
                    ]
        it "compiles hop1 correctly" $ do
            r <-
                runDefaultImpureCompiler $
                compileJob $ StrongCardFileReference hop1 Nothing
            r `shouldBe`
                Right
                    [ Deployment (Directions ["x/beta"] "b/one") LinkDeployment
                    , Deployment
                          (Directions ["hop2dir/y/gamma"] "c/two")
                          LinkDeployment
                    , Deployment
                          (Directions ["hop2dir/hop3dir/z/delta"] "d/three")
                          LinkDeployment
                    ]
        it "compiles root correctly" $ do
            r <-
                runDefaultImpureCompiler $
                compileJob $ StrongCardFileReference root Nothing
            r `shouldBe`
                Right
                    [ Deployment
                          (Directions ["u/alpha"] "a/zero")
                          LinkDeployment
                    , Deployment
                          (Directions ["hop1dir/x/beta"] "b/one")
                          LinkDeployment
                    , Deployment
                          (Directions ["hop1dir/hop2dir/y/gamma"] "c/two")
                          LinkDeployment
                    , Deployment
                          (Directions
                               ["hop1dir/hop2dir/hop3dir/z/delta"]
                               "d/three")
                          LinkDeployment
                    ]

exactTests :: Spec
exactTests = do
    describe "exact tests" $ do
        dir <- runIO $ resolveDir' "test_resources/exact_compile_test_src"
        forFileInDirss [dir] $ \fp -> do
            if fileExtension fp == Just ".res"
                then return ()
                else do
                    it (toFilePath fp) $ do
                        let orig = fp
                        result <- parseAbsFile $ toFilePath fp <.> "res"
                        ads <-
                            runDefaultImpureCompiler $
                            compileJob $ StrongCardFileReference orig Nothing
                        eds <- runDefaultImpureCompiler $ inputCompiled result
                        ads `shouldBe` eds

hopTestDir :: Path Rel Dir
hopTestDir = $(mkRelDir "hop_test")

compilerBlackBoxTests :: Spec
compilerBlackBoxTests = do
    tr <- runIO $ resolveDir' "test_resources"
    describe "Succesful compile examples" $ do
        let dirs = map (tr </>) [shouldCompileDir, hopTestDir]
        forFileInDirss dirs $ \f -> do
            it (toFilePath f) $ do
                r <-
                    runDefaultImpureCompiler $
                    compileJob $ StrongCardFileReference f Nothing
                r `shouldSatisfy` isRight
    describe "Unsuccesfull compile examples" $ do
        let dirs = map (tr </>) [shouldNotParseDir, shouldNotCompileDir]
        forFileInDirss dirs $ \f -> do
            it (toFilePath f) $ do
                r <-
                    runDefaultImpureCompiler $
                    compileJob $ StrongCardFileReference f Nothing
                r `shouldSatisfy` isLeft
