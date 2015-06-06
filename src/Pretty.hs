{-# LANGUAGE OverloadedStrings #-}
module Pretty where

import           Text.PrettyPrint

import           Constants
import           Types

formatCards :: [Card] -> String
formatCards cs = render $ hsep $ map pretty cs

indent = nest 4

class Pretty a where
    pretty :: a -> Doc

instance Pretty Card where
    pretty (Card name _ decs) = text name <+> hsep (map pretty decs)

instance Pretty Declaration where
    pretty (SparkOff st) = hsep [text keywordSpark, pretty st]
    pretty (Deploy src dst kind) = hsep [text src, pretty kind, text dst]
    pretty (IntoDir dir) = hsep [text keywordInto, text dir]
    pretty (OutofDir dir) = hsep [text keywordOutof, text dir]
    pretty (DeployKindOverride kind) = hsep [text keywordKindOverride, pretty kind]
    pretty (Block ds) = braces . indent . hsep $ map pretty ds

instance Pretty DeploymentKind where
    pretty LinkDeployment = text linkKindSymbol
    pretty CopyDeployment = text copyKindSymbol
    pretty UnspecifiedDeployment = text unspecifiedKindSymbol

instance Pretty SparkTarget where
    pretty (TargetGit repo) = hsep [text keywordGit, text $ show repo]
    pretty (TargetCardName name) = hsep [text keywordCard, text name]
