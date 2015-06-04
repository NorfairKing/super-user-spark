module Parser where

import           Text.Parsec                   (Parsec)
import           Text.ParserCombinators.Parsec

parseFile :: FilePath -> IO (Either ParseError [Card])
parseFile file = do
    ls <- readFile file
    let p = runParser sparkFile (initialState file) file ls
    return p

type SparkParser = Parsec String ParseState
gets :: (ParseState -> a) -> SparkParser a
gets f = do
    s <- getState
    return $ f s

data ParseState = ParseState {
        stateCurrentFile :: FilePath
    }

initialState :: FilePath -> ParseState
initialState file = ParseState {
        stateCurrentFile = file
    }

---[ Types ]---

type CardIdentifier = String
type CardName = Maybe CardIdentifier
type Source = FilePath
type Destination = FilePath
type Directory = FilePath
type Repo = String

data Card = Card CardName [Declaration]
    deriving (Show, Eq)

data DeploymentKind = LinkDeployment
                    | CopyDeployment
                    | UnspecifiedDeployment
    deriving (Show, Eq)

data SparkTarget = TargetGit Repo
                 | TargetCardName CardIdentifier
    deriving (Show, Eq)

data Declaration = SparkOff SparkTarget
                 | Deploy Source Destination DeploymentKind
                 | IntoDir Directory -- Relative
    deriving (Show, Eq)


---[ Parsing ]---

sparkFile :: SparkParser [Card]
sparkFile = do
    clean <- eatComments
    setInput clean
    sepEndBy1 card whitespace


card :: SparkParser Card
card = do
    whitespace
    string "card"
    whitespace
    name <- cardName
    whitespace
    content <- cardContent
    return $ Card name content

cardName :: SparkParser CardName
cardName = optionMaybe cardIdentifier

cardIdentifier :: SparkParser CardIdentifier
cardIdentifier = try quotedIdentifier <|> try simpelIdentifier
    where
        simpelIdentifier = many1 $ noneOf " \t\n\r\"{"
        quotedIdentifier = inQuotes $ many $ noneOf "\"\n\r"

cardContent :: SparkParser [Declaration]
cardContent = inBraces $ do
    whitespace
    content <- declaration `sepEndBy` delim
    whitespace
    return content

declaration :: SparkParser Declaration
declaration = inWhiteSpace $ try deployment <|> try sparkOff <|> try intoDir

sparkOff :: SparkParser Declaration
sparkOff = do
    string "spark" -- placeholder
    target <- try sparkCard <|> try sparkGit
    return $ SparkOff target

sparkCard :: SparkParser SparkTarget
sparkCard = do
    string "Card"
    ident <- cardIdentifier
    return $ TargetCardName ident

sparkGit :: SparkParser SparkTarget
sparkGit = do
    string "Git"
    r <- repo
    return $ TargetGit r

intoDir :: SparkParser Declaration
intoDir = do
    string "into"
    linespace
    dir <- directory
    return $ IntoDir dir

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
        plain = many1 $ noneOf " \t\n\r"
        quoted = inQuotes $ many $ noneOf ['\"','\n','\r']

directory :: SparkParser Directory
directory = filepath

repo :: SparkParser Repo
repo = filepath

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
delim = try eol <|> delimiter
    where
        delimiter = string ";"

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
