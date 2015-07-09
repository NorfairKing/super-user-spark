module Parser where
import           System.FilePath.Posix (takeExtension)
import           Text.Parsec
import           Text.Parsec.String

import           Data.List             (find, isSuffixOf)

import           Constants
import           Git
import           Types

parseStartingCardReference :: StartingSparkReference -> Sparker [Card]
parseStartingCardReference (StartRepo (CardRepoReference repo mb mfr)) = do
    case mb of
        Nothing -> return ()
        Just gb -> runGitRepoController repo $ checkout gb
    case mfr of
        Nothing -> do
            fps <- runGitRepoController repo $ listRepoDir "."
            case filter (\f -> takeExtension f == '.':sparkExtension) fps of
                [] -> throwError $ UnpredictedError $ unwords ["Didn't find any", "\"" ++ sparkExtension ++ "\"", "files in", show repo]
                (h:_) -> parseStartingCardReference $ StartFile $ CardFileReference h Nothing
        Just (CardFileReference fp mnr) -> do
            gfp <- runGitRepoController repo $ repoFilePath fp
            parseStartingCardReference $ StartFile $ CardFileReference gfp mnr


parseStartingCardReference (StartFile (CardFileReference fp mnr)) = do
    css <- parseFile fp
    case mnr of
        Nothing -> return css
        Just (CardNameReference cn) ->
            case find (\s -> card_name s == cn) css of
                Nothing -> throwError $ UnpredictedError $ unwords ["Did't find card", "\"" ++ cn ++ "\"", "in", fp]
                Just c  -> return [c]



parseFile :: FilePath -> Sparker [Card]
parseFile file = do
    str <- liftIO $ readFile file
    case parse sparkFile file str of
        Left pe -> throwError $ ParseError pe
        Right cs -> return cs


parseCardReference :: String -> Either ParseError StartingSparkReference
parseCardReference = parse startingCardReference "Argument String"


---[ Parsing ]---

sparkFile :: Parser [Card]
sparkFile = do
    clean <- eatComments
    setInput clean
    resetPosition
    card `sepEndBy1` whitespace

resetPosition :: Parser ()
resetPosition = do
    pos <- getPosition
    setPosition $ setSourceColumn (setSourceLine pos 1) 1


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
    Block ds <- block
    whitespace
    fp <- getFile
    return $ Card name fp ds

cardName :: Parser CardName
cardName = try quotedIdentifier <|> try plainIdentifier <?> "card name"

declarations :: Parser [Declaration]
declarations = (inLineSpace declaration) `sepEndBy` delim

declaration :: Parser Declaration
declaration = choice $ map try
    [
      block
    , alternatives
    , sparkOff
    , intoDir
    , outOfDir
    , deploymentKindOverride
    , deployment
    ]

block :: Parser Declaration
block = do
    ds <- inBraces $ inWhiteSpace declarations
    return $ Block ds
    <?> "block"

sparkOff :: Parser Declaration
sparkOff = do
    string keywordSpark
    linespace
    ref <- cardReference
    return $ SparkOff ref
    <?> "sparkoff"

startingCardReference :: Parser StartingSparkReference
startingCardReference = try goFile <|> try goRepo <?> "starting card reference"
  where
    goFile = cardFileReference >>= return . StartFile
    goRepo = cardRepoReference >>= return . StartRepo

cardReference :: Parser CardReference
cardReference = try goName <|> try goFile <|> try goRepo
    <?> "card reference"
  where
    goName = cardNameReference >>= return . CardName
    goFile = cardFileReference >>= return . CardFile
    goRepo = cardRepoReference >>= return . CardRepo

cardNameReference :: Parser CardNameReference
cardNameReference = do
    string keywordCard
    linespace
    name <- cardName
    return $ CardNameReference name
    <?> "card name reference"

cardFileReference :: Parser CardFileReference
cardFileReference = do
    string keywordFile
    linespace
    fp <- filepath
    linespace
    mn <- optionMaybe $ try cardName
    return $ case mn of
        Nothing -> CardFileReference fp Nothing
        Just cn  -> CardFileReference fp (Just $ CardNameReference cn)
    <?> "card file reference"

cardRepoReference :: Parser CardRepoReference
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
    return $ case mfpcn of
        Nothing -> CardRepoReference repo mb Nothing
        Just (fp, mcn) -> case mcn of
            Nothing -> CardRepoReference repo mb (Just $ CardFileReference fp Nothing)
            Just cn -> CardRepoReference repo mb (Just $ CardFileReference fp $ Just $ CardNameReference cn)
    <?> "card git reference"
  where
    branch :: Parser Branch
    branch = cardName -- Fix this is this is not quite enough.

intoDir :: Parser Declaration
intoDir = do
    string keywordInto
    linespace
    dir <- directory
    return $ IntoDir dir
    <?> "into directory declaration"

outOfDir :: Parser Declaration
outOfDir = do
    string keywordOutof
    linespace
    dir <- directory
    return $ OutofDir dir
    <?> "outof directory declaration"

deploymentKindOverride :: Parser Declaration
deploymentKindOverride = do
    string keywordKindOverride
    linespace
    kind <- try copy <|> link
    return $ DeployKindOverride kind
    <?> "deployment kind override"
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
    <?> "deployment"

deploymentKind :: Parser (Maybe DeploymentKind)
deploymentKind = try link <|> try copy <|> def
    <?> "deployment kind"
    where
        link = string linkKindSymbol >> return (Just LinkDeployment)
        copy = string copyKindSymbol >> return (Just CopyDeployment)
        def  = string unspecifiedKindSymbol >> return Nothing

alternatives :: Parser Declaration
alternatives = do
    string keywordAlternatives
    linespace
    ds <- directory `sepBy1` linespace
    return $ Alternatives ds

-- [ FilePaths ]--

filepath :: Parser FilePath
filepath = try quotedIdentifier <|> plainIdentifier

directory :: Parser Directory
directory = do
    d <- filepath
    return $ if "/" `isSuffixOf` d
    then init d
    else d
    <?> "directory"


--[ Comments ]--

comment :: Parser String
comment = lineComment <|> blockComment

lineComment :: Parser String
lineComment = do
    skip $ string lineCommentStr
    anyChar `manyTill` try eol

blockComment :: Parser String
blockComment = do
    skip $ string start
    anyChar `manyTill` try (string stop)
  where (start, stop) = blockCommentStrs



notComment :: Parser String
notComment = manyTill anyChar (lookAhead ((skip comment) <|> eof))

eatComments :: Parser String
eatComments = do
  optional comment
  xs <- notComment `sepBy` comment
  optional comment
  let withoutComments = concat xs
  return withoutComments


--[ Identifiers ]--

plainIdentifier :: Parser String
plainIdentifier = many1 $ noneOf $ quotesChar : lineDelimiter ++ whitespaceChars ++ bracesChars

quotedIdentifier :: Parser String
quotedIdentifier = inQuotes $ many $ noneOf $ quotesChar:endOfLineChars


--[ Delimiters ]--

inBraces :: Parser a -> Parser a
inBraces = between (char '{') (char '}')

inQuotes :: Parser a -> Parser a
inQuotes = between (char quotesChar) (char quotesChar)

delim :: Parser String
delim = try (string lineDelimiter) <|> go
  where
    go = do
        e <- eol
        ws <- whitespace
        return $ e ++ ws


--[ Whitespace ]--

inLineSpace :: Parser a -> Parser a
inLineSpace = between linespace linespace

inWhiteSpace :: Parser a -> Parser a
inWhiteSpace = between whitespace whitespace

linespace :: Parser String
linespace = many $ oneOf linespaceChars

whitespace :: Parser String
whitespace = many $ oneOf whitespaceChars

eol :: Parser String
eol =   try (string "\n\r")
    <|> try (string "\r\n")
    <|> try (string "\n")
    <|> string "\r"
    <?> "end of line"


--[ Git ]--

gitRepo :: Parser GitRepo
gitRepo = do
    prot <- gitProtocol
    case prot of
        HTTPS -> do
            host <- manyTill (noneOf "/") (string "/")
            path <- many (noneOf " :")
            return $ GitRepo {
                    repo_protocol = HTTPS, repo_host = host, repo_path = path
                }
        Git   -> do
            host <- manyTill (noneOf ":") (string ":")
            path <- manyTill anyToken (string ".git")
            return $ GitRepo {
                    repo_protocol = Git, repo_host = host, repo_path = path
                }

gitProtocol :: Parser GitProtocol
gitProtocol = https <|> git
  where
    https = string "https://" >> return HTTPS
    git   = string "git@"     >> return Git

--[ Utils ]--

skip :: Parser a -> Parser ()
skip p = p >> return ()
