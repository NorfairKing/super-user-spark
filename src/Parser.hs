module Parser where

import           Text.Parsec
import           Text.Parsec.String

import           Constants
import           Types

parseFile :: FilePath -> Sparker [Card]
parseFile file = do
    str <- liftIO $ readFile file
    case parse sparkFile file str of
        Left pe -> throwError $ ParseError pe
        Right cs -> return cs


---[ Git repo parsing ]---
gitRepo :: Parser GitRepo
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

gitProtocol :: Parser GitProtocol
gitProtocol = https <|> git
  where
    https = string "https://" >> return HTTPS
    git   = string "git@"     >> return Git



---[ Parsing ]---

sparkFile :: Parser [Card]
sparkFile = do
    clean <- eatComments
    setInput clean
    pos <- getPosition
    setPosition $ setSourceColumn (setSourceLine pos 1) 1
    sepEndBy1 card whitespace


getFile :: Parser FilePath
getFile = do
    pos <- getPosition
    let file = sourceName pos
    return file

card :: Parser Card
card = do
    whitespace
    string keywordCard
    whitespace
    name <- cardName
    whitespace
    content <- cardContent
    whitespace
    fp <- getFile
    return $ Card name fp content

cardName :: Parser CardName
cardName = try quotedIdentifier <|> try plainIdentifier <?> "card name"

cardContent :: Parser [Declaration]
cardContent = declarations

declarations :: Parser [Declaration]
declarations = inBraces $ inWhiteSpace $ declaration `sepEndBy` delim

declaration :: Parser Declaration
declaration = inLineSpace $ try block <|> try sparkOff <|> try intoDir <|> try outOfDir <|> try deploymentKindOverride <|> try deployment

block :: Parser Declaration
block = do
    ds <- declarations
    return $ Block ds

sparkOff :: Parser Declaration
sparkOff = do
    string keywordSpark
    linespace
    ref <- cardReference
    return $ SparkOff ref

cardReference :: Parser CardReference
cardReference = try cardNameReference <|> try cardFileReference <|> try cardRepoReference

cardNameReference :: Parser CardReference
cardNameReference = do
    string keywordCard
    linespace
    name <- cardName
    return $ CardName name

cardFileReference :: Parser CardReference
cardFileReference = do
    string keywordFile
    linespace
    fp <- filepath
    linespace
    mn <- optionMaybe $ try cardName
    return $ CardFile fp mn

cardRepoReference :: Parser CardReference
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
    branch :: Parser Branch
    branch = cardName -- Fix this is this is not quite enough.

intoDir :: Parser Declaration
intoDir = do
    string keywordInto
    linespace
    dir <- directory
    return $ IntoDir dir

outOfDir :: Parser Declaration
outOfDir = do
    string keywordOutof
    linespace
    dir <- directory
    return $ OutofDir dir

deploymentKindOverride :: Parser Declaration
deploymentKindOverride = do
    string keywordKindOverride
    linespace
    kind <- copy <|> link
    return $ DeployKindOverride kind
  where
    copy = string keywordCopy >> return CopyDeployment
    link = string keywordLink >> return LinkDeployment

deployment :: Parser Declaration
deployment = do
    source <- filepath
    linespace
    kind <- deploymentKind
    linespace
    dest <- filepath
    return $ Deploy source dest kind

deploymentKind :: Parser DeploymentKind
deploymentKind = try link <|> try copy <|> def
    where
        link = string linkKindSymbol >> return LinkDeployment
        copy = string copyKindSymbol >> return CopyDeployment
        def  = string unspecifiedKindSymbol >> return UnspecifiedDeployment

filepath :: Parser FilePath
filepath = try quotedIdentifier <|> plainIdentifier

directory :: Parser Directory
directory = filepath

--[ Comments ]--
comment :: Parser ()
comment = lineComment -- <|> blockComment
    where
        lineComment :: Parser ()
        lineComment = do
            string lineCommentStr
            manyTill anyChar (try $ lookAhead eol)
            return ()

notComment :: Parser String
notComment = manyTill anyChar (lookAhead (comment <|> eof))

eatComments :: Parser String
eatComments = do
  optional comment
  xs <- sepBy notComment comment
  optional comment
  let withoutComments = concat xs
  return withoutComments

--[ Whitespace ]--

inBraces :: Parser a -> Parser a
inBraces = between (char '{') (char '}')

inQuotes :: Parser a -> Parser a
inQuotes = between (char quotesChar) (char quotesChar)

inLineSpace :: Parser a -> Parser a
inLineSpace = between linespace linespace

inWhiteSpace :: Parser a -> Parser a
inWhiteSpace = between whitespace whitespace

delim :: Parser String
delim = string lineDelimiter <|> whitespace

linespace :: Parser String
linespace = many $ oneOf linespaceChars

whitespace :: Parser String
whitespace = many $ oneOf whitespaceChars

plainIdentifier :: Parser String
plainIdentifier = many1 $ noneOf $ quotesChar : lineDelimiter ++ whitespaceChars ++ bracesChars

quotedIdentifier :: Parser String
quotedIdentifier = inQuotes $ many $ noneOf $ quotesChar:endOfLineChars

word :: Parser String
word = many1 letter

eol :: Parser String
eol =   try (string "\n\r")
    <|> try (string "\r\n")
    <|> try (string "\n")
    <|> string "\r"
    <?> "end of line"
