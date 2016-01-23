{-# LANGUAGE OverloadedStrings #-}
module Parser.Types where

import           CoreTypes

-- * Cards

type CardName = String
type Source = FilePath
type Destination = FilePath
type Directory = FilePath

data Card = Card {
        card_name    :: CardName
    ,   card_path    :: FilePath
    ,   card_content :: Declaration
    } deriving (Show, Eq)

-- * Declarations

-- | A declaration in a card
data Declaration = SparkOff CardReference -- ^ Spark off another card
                 | Deploy Source Destination (Maybe DeploymentKind) -- ^ Deploy from source to destination
                 | IntoDir Directory -- ^ Deploy into a directory
                 | OutofDir Directory -- ^ Deploy outof a directory
                 | DeployKindOverride DeploymentKind -- ^ Override the deployment kind
                 | Alternatives [Directory] -- ^ Provide a list of alternative sources
                 | Block [Declaration] -- ^ A scoped block of declarations
    deriving (Show, Eq)

data CardNameReference = CardNameReference CardName
    deriving (Show, Eq)

-- Reference a card by the file it's in and therein potentially by a name reference
data CardFileReference = CardFileReference FilePath (Maybe CardNameReference)
    deriving (Show, Eq)

data CardReference = CardFile CardFileReference
                   | CardName CardNameReference
    deriving (Show, Eq)

