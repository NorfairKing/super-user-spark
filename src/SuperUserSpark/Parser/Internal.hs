module SuperUserSpark.Parser.Internal where

import Import

import Data.List (isSuffixOf)
import SuperUserSpark.Constants
import SuperUserSpark.CoreTypes
import SuperUserSpark.Language.Types
import Text.Parsec
import Text.Parsec.String

parseCardFile :: Path Abs File -> String -> Either ParseError SparkFile
parseCardFile f s = SparkFile f <$> parseFromSource sparkFile f s

parseFromSource :: Parser a -> Path Abs File -> String -> Either ParseError a
parseFromSource parser file = parse parser $ toFilePath file

--[ Language ]--
sparkFile :: Parser [Card]
sparkFile = do
    clean <- eatComments
    setInput clean
    resetPosition
    cards

cards :: Parser [Card]
cards = card `sepEndBy1` whitespace

resetPosition :: Parser ()
resetPosition = do
    pos <- getPosition
    setPosition $ setSourceColumn (setSourceLine pos 1) 1

card :: Parser Card
card = do
    whitespace
    void $ string keywordCard
    whitespace
    name <- cardNameP
    whitespace
    b <- block
    whitespace
    return $ Card name b

declarations :: Parser [Declaration]
declarations = inLineSpace declaration `sepEndBy` delim

declaration :: Parser Declaration
declaration =
    choice $
    map
        try
        [ block
        , alternatives
        , sparkOff
        , intoDir
        , outOfDir
        , deploymentKindOverride
        , deployment
        ]

block :: Parser Declaration
block =
    do ds <- inBraces $ inWhiteSpace declarations
       return $ Block ds
       <?> "block"

sparkOff :: Parser Declaration
sparkOff =
    do void $ string keywordSpark
       linespace
       ref <- cardReference
       return $ SparkOff ref
       <?> "sparkoff"

compilerCardReference :: Parser CardFileReference
compilerCardReference = unprefixedCardFileReference

compiledCardReference :: Parser FilePath
compiledCardReference = do
    void $ string "compiled"
    void linespace
    filepath

cardReference :: Parser CardReference
cardReference = try goName <|> try goFile <?> "card reference"
  where
    goName = CardName <$> cardNameReference
    goFile = CardFile <$> cardFileReference

cardNameReference :: Parser CardNameReference
cardNameReference =
    do void $ string keywordCard
       linespace
       name <- cardNameP
       return $ CardNameReference name
       <?> "card name reference"

cardNameP :: Parser CardName
cardNameP = identifier <?> "card name"

cardFileReference :: Parser CardFileReference
cardFileReference = do
    void $ string keywordFile
    void linespace
    unprefixedCardFileReference

unprefixedCardFileReference :: Parser CardFileReference
unprefixedCardFileReference =
    do fp <- filepath
       linespace
       mn <- optionMaybe $ try cardNameP
       return $
           case mn of
               Nothing -> CardFileReference fp Nothing
               Just cn -> CardFileReference fp (Just $ CardNameReference cn)
       <?> "card file reference"

intoDir :: Parser Declaration
intoDir =
    do void $ string keywordInto
       linespace
       dir <- directory
       return $ IntoDir dir
       <?> "into directory declaration"

outOfDir :: Parser Declaration
outOfDir =
    do void $ string keywordOutof
       linespace
       dir <- directory
       return $ OutofDir dir
       <?> "outof directory declaration"

deploymentKindOverride :: Parser Declaration
deploymentKindOverride =
    do void $ string keywordKindOverride
       linespace
       kind <- try copy <|> link
       return $ DeployKindOverride kind
       <?> "deployment kind override"
  where
    copy = string keywordCopy >> return CopyDeployment
    link = string keywordLink >> return LinkDeployment

shortDeployment :: Parser Declaration
shortDeployment = do
    source <- try directory <|> filepath
    return $ Deploy source source Nothing

longDeployment :: Parser Declaration
longDeployment = do
    source <- filepath
    linespace
    kind <- deploymentKind
    linespace
    dest <- filepath
    return $ Deploy source dest kind

deployment :: Parser Declaration
deployment = try longDeployment <|> shortDeployment <?> "deployment"

deploymentKind :: Parser (Maybe DeploymentKind)
deploymentKind = try link <|> try copy <|> def <?> "deployment kind"
  where
    link = string linkKindSymbol >> return (Just LinkDeployment)
    copy = string copyKindSymbol >> return (Just CopyDeployment)
    def = string unspecifiedKindSymbol >> return Nothing

alternatives :: Parser Declaration
alternatives = do
    void $ string keywordAlternatives
    linespace
    ds <- directory `sepBy1` linespace
    return $ Alternatives ds

-- [ FilePaths ]--
filepath :: Parser FilePath
filepath = do
    i <- identifier <?> "Filepath"
    if "/" `isSuffixOf` i
        then unexpected "slash at the end"
        else return i

directory :: Parser Directory
directory = filepath <?> "Directory"

--[ Comments ]--
comment :: Parser String
comment = try lineComment <|> try blockComment <?> "Comment"

lineComment :: Parser String
lineComment =
    (<?> "Line comment") $ do
        void $ try $ string lineCommentStr
        anyChar `manyTill` eol

blockComment :: Parser String
blockComment =
    (<?> "Block comment") $ do
        void $ try $ string start
        anyChar `manyTill` try (string stop)
  where
    (start, stop) = blockCommentStrs

notComment :: Parser String
notComment = manyTill anyChar (lookAhead (void comment <|> eof))

eatComments :: Parser String
eatComments = do
    optional comment
    xs <- notComment `sepBy` comment
    optional comment
    let withoutComments = concat xs
    return withoutComments

--[ Identifiers ]--
identifier :: Parser String
identifier = try quotedIdentifier <|> plainIdentifier

plainIdentifier :: Parser String
plainIdentifier =
    many1 $
    noneOf $ quotesChar : lineDelimiter ++ whitespaceChars ++ bracesChars

quotedIdentifier :: Parser String
quotedIdentifier = inQuotes $ many $ noneOf $ quotesChar : endOfLineChars

--[ Delimiters ]--
inBraces :: Parser a -> Parser a
inBraces = between (char '{') (char '}')

inQuotes :: Parser a -> Parser a
inQuotes = between (char quotesChar) (char quotesChar)

delim :: Parser ()
delim = try (void $ string lineDelimiter) <|> go
  where
    go = do
        eol
        whitespace

--[ Whitespace ]--
inLineSpace :: Parser a -> Parser a
inLineSpace = between linespace linespace

inWhiteSpace :: Parser a -> Parser a
inWhiteSpace = between whitespace whitespace

linespace :: Parser ()
linespace = void $ many $ oneOf linespaceChars

whitespace :: Parser ()
whitespace = void $ many $ oneOf whitespaceChars

eol :: Parser ()
eol = void newline_
  where
    newline_ =
        try (string "\r\n") <|> try (string "\n") <|>
        string "\r" <?> "end of line"
