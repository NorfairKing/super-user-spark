module Arguments where

import           Options.Applicative
import           System.Environment  (getArgs)
import           System.Exit         (die)

import           Types

getInstructions :: IO Instructions
getInstructions = do
  mins <- fmap transformOptions getOptions
  case mins of
    Right ins -> return ins
    Left err  -> die $ "Failed to parse instructions\n" ++ err

transformOptions :: Options -> Either String Instructions
transformOptions opts = undefined

getOptions :: IO Options
getOptions = do
  args <- getArgs
  let result = runOptionsParser args
  handleParseResult result

runOptionsParser :: [String] -> ParserResult Options
runOptionsParser strs = execParserPure prefs optionsParser strs
  where
    prefs = ParserPrefs {
            prefMultiSuffix = "SPARK"    -- ^ metavar suffix for multiple options
          , prefDisambiguate = True    -- ^ automatically disambiguate abbreviations (default: False)
          , prefShowHelpOnError = True -- ^ always show help text on parse errors (default: False)
          , prefBacktrack = True       -- ^ backtrack to parent parser when a subcommand fails (default: True)
          , prefColumns = 80           -- ^ number of columns in the terminal, used to format the help page (default: 80)
        }

optionsParser :: ParserInfo Options
optionsParser = info (helper <*> parseOptions) help
  where
    help = fullDesc <> progDesc description
    description = "Super User Spark"

parseOptions :: Parser Options
parseOptions = Options
    <$> parseCommand
    <*> parseGlobalOptions

parseCommand :: Parser Command
parseCommand = hsubparser $ mconcat
    [
      command "parse"   parseParse
    , command "format"  parseFormat
    , command "compile" parseCompile
    , command "check"   parseCheck
    , command "deploy"  parseDeploy
    ]

parseParse :: ParserInfo Command
parseParse = info parser modifier
  where
    parser = CommandParse <$> strArgument (metavar "FILE" <> help "the file to parse")
    modifier = fullDesc
            <> progDesc "Parse a spark file and check for syntactic errors."

parseFormat :: ParserInfo Command
parseFormat = info parser modifier
  where
    parser = CommandFormat <$> strArgument (metavar "FILE" <> help "the file to format")
    modifier = fullDesc
            <> progDesc "Format a spark file."

parseCompile :: ParserInfo Command
parseCompile = info parser modifier
  where
    parser = CommandCompile <$> strArgument (metavar "CARD" <> help "the card file to compile")
    modifier = fullDesc
            <> progDesc "Compile a spark card."

parseCheck :: ParserInfo Command
parseCheck = info parser modifier
  where
    parser = CommandCheck <$> strArgument (metavar "CARD" <> help "the card to check")
    modifier = fullDesc
            <> progDesc "Check the deployment of a spark card."

parseDeploy :: ParserInfo Command
parseDeploy = info parser modifier
  where
    parser = CommandDeploy <$> strArgument (metavar "CARD" <> help "the card to deploy")
    modifier = fullDesc
            <> progDesc "Deploy a spark card."


parseGlobalOptions :: Parser GlobalOptions
parseGlobalOptions = GlobalOptions
  <$> flag True False -- Backwards
    ( long "no-line-up"
      <> help "Don't line up arrows" )
  <*> option auto
    ( long "indent"
      <> help "How many spaces to use for indentation when formatting" )
  <*> flag True False -- Backwards
    ( long "no-trailing-newline"
      <> help "Don't add a trailing newline to a formatted file" )
  <*> switch
    ( long "always-quote"
      <> help "Always quote file names" )
