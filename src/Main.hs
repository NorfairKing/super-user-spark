module Main where

import           System.Environment            (getArgs)

--import           Text.ParserCombinators.Parsec (ParseError, Parser (..),
                                                --anyChar, between, char, letter,
                                                --many, many1, noneOf, option,
                                                --optional, parse, spaces, string)
                                                --
import           Text.Parsec                   (Parsec)
import           Text.ParserCombinators.Parsec

main = do
    args <- getArgs
    case args of
     (file:[]) -> do
        cs <- parseFile file
        print cs
     _ -> putStrLn "error parsing arguments"

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
        stateDeploymentKind :: DeploymentKind
    ,   stateCurrentFile    :: FilePath
    }

initialState :: FilePath -> ParseState
initialState file = ParseState {
        stateDeploymentKind = LinkDeployment
    ,   stateCurrentFile = file
    }

---[ Types ]---

type CardName = String
type Source = FilePath
type Destination = FilePath

data Card = Card CardName [Declaration]
    deriving (Show, Eq)

data DeploymentKind = LinkDeployment
                    | CopyDeployment
    deriving (Show, Eq)
data Declaration = SparkOff String -- Dunno
                 | Deploy Source Destination DeploymentKind
    deriving (Show, Eq)


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
    return $ Card name content

cardName :: SparkParser CardName
cardName = do
    filename <- gets stateCurrentFile
    name <- option filename $ many1 letter <|> inQuotes (many1 $ noneOf "\"")
    return name

cardContent :: SparkParser [Declaration]
cardContent = inBraces $ do
    whitespace
    content <- declaration `sepEndBy` delim
    whitespace
    return content

declaration :: SparkParser Declaration
declaration = do
    linespace
    dec <- try deployment <|> sparkOff
    linespace
    return dec

sparkOff :: SparkParser Declaration
sparkOff = do
    str <- string "spark" -- placeholder
    return $ SparkOff str

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
        def = do
            string "->"
            kind <- gets stateDeploymentKind
            return kind

filepath :: SparkParser FilePath
filepath = try plain <|> quoted
    where
        plain = many $ noneOf " \t\n\r"
        quoted = inQuotes $ many $ noneOf " \"\t\n\r"

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

--[ Whitespace ]--

inBraces :: SparkParser a -> SparkParser a
inBraces = between (char '{') (char '}')

inQuotes :: SparkParser a -> SparkParser a
inQuotes = between (char '"') (char '"')

inSpaces :: SparkParser a -> SparkParser a
inSpaces c = do
    spaces
    a <- c
    spaces
    return a

delim :: SparkParser String
delim = try (delimiter >> eol) <|> try eol <|> delimiter
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
