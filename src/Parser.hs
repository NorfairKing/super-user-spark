module Parser where

import           Text.Parsec

import           Constants
import           Git
import           Types

parseFile :: FilePath -> Sparker [Card]
parseFile file = do
    ls <- liftIO $ readFile file
    runSparkParser (initialState file) sparkFile ls

initialState :: FilePath -> ParseState
initialState file = ParseState {
        state_starting_file = file
    ,   state_current_file = file
    }


---[ Git repo parsing ]---
gitRepo :: SparkParser GitRepo
gitRepo = do
    prot <- gitProtocol
    case prot of
        HTTPS -> do
            host <- manyTill anyToken (string "/")
            path <- many anyToken
            return $ GitRepo {
                    repo_protocol = HTTPS, repo_host = host, repo_path = path
                }
        Git   -> do
            host <- manyTill anyToken (string ":")
            path <- manyTill anyToken (string ".git")
            return $ GitRepo {
                    repo_protocol = Git, repo_host = host, repo_path = path
                }

gitProtocol :: SparkParser GitProtocol
gitProtocol = https <|> git
  where
    https = string "https://" >> return HTTPS
    git   = string "git@"     >> return Git



---[ Parsing ]---

sparkFile :: SparkParser [Card]
sparkFile = do
    clean <- eatComments
    setInput clean
    pos <- getPosition
    setPosition $ setSourceColumn (setSourceLine pos 1) 1
    sepEndBy1 card whitespace


card :: SparkParser Card
card = do
    whitespace
    string keywordCard
    whitespace
    name <- cardName
    whitespace
    content <- cardContent
    whitespace
    fp <- getStates state_current_file
    return $ Card name fp content

cardName :: SparkParser CardName
cardName = try quotedIdentifier <|> try plainIdentifier <?> "card name"

cardContent :: SparkParser [Declaration]
cardContent = declarations

declarations :: SparkParser [Declaration]
declarations = inBraces $ inWhiteSpace $ declaration `sepEndBy` delim

declaration :: SparkParser Declaration
declaration = inLineSpace $ try block <|> try sparkOff <|> try intoDir <|> try outOfDir <|> try deploymentKindOverride <|> try deployment

block :: SparkParser Declaration
block = do
    ds <- declarations
    return $ Block ds

sparkOff :: SparkParser Declaration
sparkOff = do
    string keywordSpark
    linespace
    ref <- cardReference
    return $ SparkOff ref

cardReference :: SparkParser CardReference
cardReference = try cardNameReference <|> try cardFileReference <|> try cardRepoReference

cardNameReference :: SparkParser CardReference
cardNameReference = do
    string keywordCard
    linespace
    name <- cardName
    return $ CardName name

cardFileReference :: SparkParser CardReference
cardFileReference = do
    string keywordFile
    linespace
    fp <- filepath
    linespace
    mn <- optionMaybe $ try cardName
    return $ CardFile fp mn

cardRepoReference :: SparkParser CardReference
cardRepoReference = do
    string keywordGit
    linespace
    repo <- gitRepo
    mb <- optionMaybe $ try $ do
        string branchDelimiter
        branch
    mfpcn <- optionMaybe $ try $ do
        linespace
        fp <- filepath
        linespace
        mcn <- optionMaybe $ try cardName
        return (fp, mcn)
    return $ CardRepo repo mb mfpcn
  where
    branch :: SparkParser Branch
    branch = cardName -- Fix this is this is not quite enough.

intoDir :: SparkParser Declaration
intoDir = do
    string keywordInto
    linespace
    dir <- directory
    return $ IntoDir dir

outOfDir :: SparkParser Declaration
outOfDir = do
    string keywordOutof
    linespace
    dir <- directory
    return $ OutofDir dir

deploymentKindOverride :: SparkParser Declaration
deploymentKindOverride = do
    string keywordKindOverride
    linespace
    kind <- copy <|> link
    return $ DeployKindOverride kind
  where
    copy = string keywordCopy >> return CopyDeployment
    link = string keywordLink >> return LinkDeployment

deployment :: SparkParser Declaration
deployment = do
    source <- filepath
    linespace
    kind <- deploymentKind
    linespace
    dest <- filepath
    return $ Deploy source dest kind

deploymentKind :: SparkParser DeploymentKind
deploymentKind = try link <|> try copy <|> def
    where
        link = string linkKindSymbol >> return LinkDeployment
        copy = string copyKindSymbol >> return CopyDeployment
        def  = string unspecifiedKindSymbol >> return UnspecifiedDeployment

filepath :: SparkParser FilePath
filepath = try quotedIdentifier <|> plainIdentifier

directory :: SparkParser Directory
directory = filepath

--[ Comments ]--
comment :: SparkParser ()
comment = lineComment -- <|> blockComment
    where
        lineComment :: SparkParser ()
        lineComment = do
            string lineCommentStr
            manyTill anyChar (try $ lookAhead eol)
            return ()

notComment :: SparkParser String
notComment = manyTill anyChar (lookAhead (comment <|> eof))

eatComments :: SparkParser String
eatComments = do
  optional comment
  xs <- sepBy notComment comment
  optional comment
  let withoutComments = concat xs
  return withoutComments


word :: SparkParser String
word = many1 letter

--[ Whitespace ]--

inBraces :: SparkParser a -> SparkParser a
inBraces = between (char '{') (char '}')

inQuotes :: SparkParser a -> SparkParser a
inQuotes = between (char quotesChar) (char quotesChar)

inLineSpace :: SparkParser a -> SparkParser a
inLineSpace = between linespace linespace

inWhiteSpace :: SparkParser a -> SparkParser a
inWhiteSpace = between whitespace whitespace

delim :: SparkParser String
delim = string lineDelimiter <|> whitespace

linespace :: SparkParser String
linespace = many $ oneOf linespaceChars

whitespace :: SparkParser String
whitespace = many $ oneOf whitespaceChars

plainIdentifier :: SparkParser String
plainIdentifier = many1 $ noneOf $ quotesChar : lineDelimiter ++ whitespaceChars ++ bracesChars

quotedIdentifier :: SparkParser String
quotedIdentifier = inQuotes $ many $ noneOf $ quotesChar:endOfLineChars

eol :: SparkParser String
eol =   try (string "\n\r")
    <|> try (string "\r\n")
    <|> try (string "\n")
    <|> string "\r"
    <?> "end of line"
