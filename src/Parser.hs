module Parser where

import           Text.ParserCombinators.Parsec

import           Types

parseFile :: FilePath -> IO (Either ParseError [Card])
parseFile file = do
    ls <- readFile file
    let p = runParser sparkFile (initialState file) file ls
    return p

gets :: (ParseState -> a) -> SparkParser a
gets f = do
    s <- getState
    return $ f s

initialState :: FilePath -> ParseState
initialState file = ParseState {
        state_current_file = file
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
sparkFile = sepEndBy1 card whitespace
    {-clean <- eatComments
    setInput clean
    sepEndBy1 card whitespace-}


card :: SparkParser Card
card = do
    whitespace
    string "card"
    whitespace
    name <- cardName
    whitespace
    content <- cardContent
    whitespace
    fp <- gets state_current_file
    return $ Card name fp content

cardName :: SparkParser CardName
cardName = try quotedIdentifier <|> try simpleIdentifier
    where
        simpleIdentifier = many1 $ noneOf " ;\t\n\r\"{}"
        quotedIdentifier = inQuotes $ many $ noneOf "\"\n\r"

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
    string "spark"
    linespace
    target <- try sparkCard <|> try sparkGit
    return $ SparkOff target

sparkCard :: SparkParser SparkTarget
sparkCard = do
    string "card"
    linespace
    ident <- cardName
    return $ TargetCardName ident

sparkGit :: SparkParser SparkTarget
sparkGit = do
    string "git"
    linespace
    r <- gitRepo
    return $ TargetGit r

intoDir :: SparkParser Declaration
intoDir = do
    string "into"
    linespace
    dir <- directory
    return $ IntoDir dir

outOfDir :: SparkParser Declaration
outOfDir = do
    string "outof"
    linespace
    dir <- directory
    return $ OutofDir dir

deploymentKindOverride :: SparkParser Declaration
deploymentKindOverride = do
    string "kind"
    linespace
    kind <- copy <|> link
    return $ DeployKindOverride kind
  where
    copy = string "copy" >> return CopyDeployment
    link = string "link" >> return LinkDeployment

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
        link = string "l->" >> return LinkDeployment
        copy = string "c->" >> return CopyDeployment
        def  = string "->"  >> return UnspecifiedDeployment

filepath :: SparkParser FilePath
filepath = try quoted <|> plain
    where
        plain = many1 $ noneOf " \t\n\r;{}"
        quoted = inQuotes $ many $ noneOf ['\"','\n','\r']

directory :: SparkParser Directory
directory = filepath

--[ Comments ]--
comment :: SparkParser ()
comment = lineComment -- <|> blockComment
    where
        lineComment :: SparkParser ()
        lineComment = do
            string "#"
            manyTill anyChar eol
            return ()

notComment :: SparkParser String
notComment = manyTill anyChar (lookAhead (comment <|> eof))

eatComments :: SparkParser String
eatComments = do
  optional comment
  xs <- sepBy notComment comment
  optional comment
  return $ concat xs


word :: SparkParser String
word = many1 letter

--[ Whitespace ]--

inBraces :: SparkParser a -> SparkParser a
inBraces = between (char '{') (char '}')

inQuotes :: SparkParser a -> SparkParser a
inQuotes = between (char '"') (char '"')

inLineSpace :: SparkParser a -> SparkParser a
inLineSpace = between linespace linespace

inWhiteSpace :: SparkParser a -> SparkParser a
inWhiteSpace = between whitespace whitespace

delim :: SparkParser String
delim = string ";" <|> whitespace

linespace :: SparkParser String
linespace = many $ oneOf " \t"

whitespace :: SparkParser String
whitespace = many $ oneOf " \t\n\r"

eol :: SparkParser String
eol =   try (string "\n\r")
    <|> try (string "\r\n")
    <|> try (string "\n")
    <|> string "\r"
    <?> "end of line"
