module PreCompiler where

import           Compiler.Types
import           Language.Types
import           Types
import           Utils

preCompileChecks :: Card -> [PreCompileError]
preCompileChecks c = runIdentity $ execWriterT $ cleanCard c

dirty :: String -> Precompiler ()
dirty s = tell ["Precompilation check failed: " ++ s]

cleanCard :: Card -> Precompiler ()
cleanCard (Card name d) = do
    cleanCardName name
    cleanDeclaration d

cleanDeclaration :: Declaration -> Precompiler ()
cleanDeclaration (Deploy src dst _) = do
    cleanFilePath src
    cleanFilePath dst

cleanDeclaration (SparkOff cr) = cleanCardReference cr
cleanDeclaration (IntoDir dir) = cleanFilePath dir
cleanDeclaration (OutofDir dir) = cleanFilePath dir
cleanDeclaration (DeployKindOverride _) = return () -- Nothing can go wrong.
cleanDeclaration (Alternatives fs) = mapM_ cleanFilePath fs
cleanDeclaration (Block ds) = mapM_ cleanDeclaration ds

cleanCardReference :: CardReference -> Precompiler ()
cleanCardReference (CardFile cfr) = cleanCardFileReference cfr
cleanCardReference (CardName cnr) = cleanCardNameReference cnr

cleanCardFileReference :: CardFileReference -> Precompiler ()
cleanCardFileReference (CardFileReference fp mcnr) = do
    cleanFilePath fp
    case mcnr of
        Nothing -> return ()
        Just cnr -> cleanCardNameReference cnr

cleanCardNameReference :: CardNameReference -> Precompiler ()
cleanCardNameReference (CardNameReference cn) = cleanCardName cn

cleanCardName :: CardName -> Precompiler ()
cleanCardName n
    | containsNewline n = dirty $ "Card name contains newline character(s): " ++ n
    | otherwise = return ()

cleanFilePath :: FilePath -> Precompiler ()
cleanFilePath [] = dirty "Empty filepath"
cleanFilePath fp
    | containsNewline fp =
        dirty $ "Filepath contains newline character(s): " ++ fp
    | containsMultipleConsequtiveSlashes fp =
        dirty $ "Filepath contains multiple consequtive slashes: " ++ fp
    | otherwise = return ()
