{-# LANGUAGE OverloadedStrings #-}
module Pretty where

import           Data.List (intersperse)

import           Constants
import           Types

formatCards :: [Card] -> Sparker String
formatCards cs = do
    initial <- initialState
    (_, res) <- runSparkFormatter initial (cards cs)
    return res

initialState :: Sparker FormatterState
initialState = return $ FormatterState {
        state_current_indent = 0
    ,   state_longest_src = 0
    }

indentation :: Int
indentation = 4

cards :: [Card] -> SparkFormatter ()
cards cs = do
    onLines card cs

onLines :: (a -> SparkFormatter ()) -> [a] -> SparkFormatter ()
onLines thingFormatter things = do
    let all = map  thingFormatter things
    sequence_ $ intersperse newline all


card :: Card -> SparkFormatter ()
card (Card name _ ds) = do
    string keywordCard
    string " "
    string name
    string " "
    declaration $ Block ds
    newline

braces :: SparkFormatter () -> SparkFormatter ()
braces f = do
    string "{"
    indent 4
    newline
    f
    indent (-4)
    newline
    string "}"

string :: String -> SparkFormatter ()
string s = tell s

newline :: SparkFormatter ()
newline = do
    string "\n"
    ci <- gets state_current_indent
    string $ replicate ci ' '

indent :: Int -> SparkFormatter ()
indent c = do
    ci <- gets state_current_indent
    modify (\s -> s {state_current_indent = ci + c})

declarations :: [Declaration] -> SparkFormatter ()
declarations = onLines declaration

declaration :: Declaration -> SparkFormatter ()
declaration (SparkOff st) = do
    string keywordSpark
    string " "
    sparkTarget st
declaration (Deploy src dst k) = do
    string src
    ls <- gets state_longest_src
    string $ replicate (ls - length src) ' '
    string " "
    kind k
    string " "
    string dst
declaration (IntoDir dir) = do
    string keywordInto
    string " "
    string dir
declaration (OutofDir dir) = do
    string keywordOutof
    string " "
    string dir
declaration (DeployKindOverride k) = do
    string keywordKindOverride
    string " "
    kind k
declaration (Block ds) = do
    ls <- gets state_longest_src
    let m = maximum $ map srcLen ds
    modify (\s -> s {state_longest_src = m} )
    braces $ declarations ds
    modify (\s -> s {state_longest_src = ls} )
  where
    srcLen (Deploy src dst k) = length src
    srcLen _ = 0

kind :: DeploymentKind -> SparkFormatter ()
kind LinkDeployment = string linkKindSymbol
kind CopyDeployment = string copyKindSymbol
kind UnspecifiedDeployment = string unspecifiedKindSymbol

sparkTarget :: SparkTarget -> SparkFormatter ()
sparkTarget (TargetGit repo) = do
    string keywordGit
    string " "
    string $ show repo
sparkTarget (TargetCardName name) = do
    string keywordCard
    string " "
    string name

{-
indent = nest indentation

inBraces d = braces $ zeroWidthText "" $+$ indent d $+$ zeroWidthText ""

class Pretty a where
    pretty :: a -> Doc

instance Pretty Card where
    pretty (Card name _ decs) = (text keywordCard <+> text name) $+$ (inBraces $ vcat $ map pretty decs)

instance Pretty Declaration where
    pretty (SparkOff st) = hsep [text keywordSpark, pretty st]
    pretty (Deploy src dst kind) = hsep [text src, pretty kind, text dst]
    pretty (IntoDir dir) = hsep [text keywordInto, text dir]
    pretty (OutofDir dir) = hsep [text keywordOutof, text dir]
    pretty (DeployKindOverride kind) = hsep [text keywordKindOverride, pretty kind]
    pretty (Block ds) = inBraces . sep $ map pretty ds

instance Pretty DeploymentKind where
    pretty LinkDeployment = text linkKindSymbol
    pretty CopyDeployment = text copyKindSymbol
    pretty UnspecifiedDeployment = text unspecifiedKindSymbol

instance Pretty SparkTarget where
    pretty (TargetGit repo) = hsep [text keywordGit, text $ show repo]
    pretty (TargetCardName name) = hsep [text keywordCard, text name]
-}
