module Arguments where

import           Data.Monoid (mconcat)
import           Options.Applicative
import           System.Environment  (getArgs)

import           Types
import           Utils


getInstructions :: IO Instructions
getInstructions = do
  mins <- fmap transformOptions getOptions
  case mins of
    Right ins -> return ins
    Left err  -> die $ "Failed to parse instructions\n" ++ err

transformOptions :: Options -> Either String Instructions
transformOptions opts = do
    config <- configFromOptions go
    let dispatch = dispatchFromCommand cmd
    return (dispatch, config)
  where
    go = opt_global opts
    cmd = opt_command opts

configFromOptions :: GlobalOptions -> Either String SparkConfig
configFromOptions go = Right conf
  where
    conf = Config {
              conf_format_lineUp              = if opt_compress go then False else opt_lineUp go
            , conf_format_indent              = if opt_compress go then 0     else opt_indent go
            , conf_format_trailingNewline     = if opt_compress go then False else opt_trailingNewline go
            , conf_format_alwaysQuote         = if opt_compress go then False else opt_alwaysQuote go
            , conf_format_oneLine             = opt_compress go
            , conf_compile_output             = opt_output go
            , conf_compile_format             = opt_format go
            , conf_compile_kind               = opt_kind go
            , conf_compile_override           = opt_overrride go
            , conf_check_thoroughness         = opt_thoroughness go
            , conf_deploy_replace_links       = opt_replace_links go || opt_replace go
            , conf_deploy_replace_files       = opt_replace_files go || opt_replace go
            , conf_deploy_replace_directories = opt_replace_directories go || opt_replace go
            , conf_debug                      = opt_debug go
          }

dispatchFromCommand :: Command -> Dispatch
dispatchFromCommand (CommandParse str)  = DispatchParse   str
dispatchFromCommand (CommandFormat str) = DispatchFormat  str
dispatchFromCommand (CommandCompile cr) = DispatchCompile cr
dispatchFromCommand (CommandCheck cr)   = DispatchCheck   cr
dispatchFromCommand (CommandDeploy cr)  = DispatchDeploy  cr

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
    description = "Super User Spark, author: Tom Sydney Kerckhove"

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
    parser = CommandCompile
      <$> argument auto
        (metavar "CARD" <> help "the card file to compile")
    modifier = fullDesc
            <> progDesc "Compile a spark card."

parseCheck :: ParserInfo Command
parseCheck = info parser modifier
  where
    parser = CommandCheck
      <$> argument auto
         (metavar "CARD" <> help "the card to check")
    modifier = fullDesc
            <> progDesc "Check the deployment of a spark card."

parseDeploy :: ParserInfo Command
parseDeploy = info parser modifier
  where
    parser = CommandDeploy
      <$> argument auto
        (metavar "CARD" <> help "the card to deploy") -- TODO more help
    modifier = fullDesc
            <> progDesc "Deploy a spark card."


parseGlobalOptions :: Parser GlobalOptions
parseGlobalOptions = GlobalOptions
  <$> flag True False -- Backwards
    ( long "no-line-up"
      <> short 'L'
      <> help "Don't line up arrows" )
  <*> option auto
    ( long "indent"
      <> short 'i'
      <> value 4
      <> metavar "NUM"
      <> help "How many spaces to use for indentation when formatting" )
  <*> flag True False -- Backwards
    ( long "no-trailing-newline"
      <> short 'N'
      <> help "Don't add a trailing newline to a formatted file" )
  <*> switch
    ( long "always-quote"
      <> short 'Q'
      <> help "Always quote file names" )
  <*> switch
    ( long "compress"
      <> short 'c'
      <> help "Compress the card as much as possible." )
  <*> option (Just <$> str)
    ( long "output"
      <> short 'o'
      <> value Nothing
      <> metavar "FILE"
      <> help "The output file for compilation" )
  <*> option auto
    ( long "format"
      <> short 'f'
      <> value FormatText
      <> metavar "FORMAT"
      <> help "Compilation format" )
  <*> option (Just <$> auto)
    ( long "kind"
      <> short 'k'
      <> value Nothing
      <> metavar "KIND"
      <> help "The kind specification for unspecified deployments (default: link)" )
  <*> option (Just <$> auto)
    ( long "override"
      <> short 'O'
      <> value Nothing
      <> metavar "KIND"
      <> help "Override every deployment to be of the given kind" )
  <*> option auto
    ( long "thoroughness"
      <> short 't'
      <> value ThoroughnessContent
      <> metavar "THOROUGHNESS"
      <> help "How thoroughly to check whether the source and destination are equal" )
  <*> switch
    ( long "replace-links"
      <> help "Replace links at deploy destinations."
    )
  <*> switch
    ( long "replace-files"
      <> help "Replace existing files at deploy destinations."
    )
  <*> switch
    ( long "replace-Directories"
      <> help "Replace existing directories at deploy destinations."
    )
  <*> switch
    ( long "replace"
      <> short 'r'
      <> help "Equivalent to --replace-files --replace-directories --replace-links"
    )
  <*> switch
    ( long "debug"
      <> help "Show al debug information." )
