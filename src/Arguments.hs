{-# LANGUAGE CPP #-}
module Arguments where

#if (MIN_VERSION_base(4,9,0))
import           Data.Monoid
#endif

import           Options.Applicative
import           System.Environment  (getArgs)

import           Config
import           Dispatch.Types
import           Types
import           Utils


getInstructions :: IO Instructions
getInstructions = do
    mins <- fmap transformOptions getOptions
    case mins of
        Right ins -> return ins
        Left err  -> die $ "Failed to parse instructions\n" ++ err

transformOptions :: Options -> Either String Instructions
transformOptions (dispatch, go) = (,) <$> pure dispatch <*> configFromOptions go

configFromOptions :: GlobalOptions -> Either String SparkConfig
configFromOptions go = Right conf
  where
    conf = defaultConfig
          { conf_compile_output             = opt_output go
          , conf_compile_kind               = opt_kind go
          , conf_compile_override           = opt_overrride go
          , conf_deploy_replace_links       = opt_replace_links go       || opt_replace go
          , conf_deploy_replace_files       = opt_replace_files go       || opt_replace go
          , conf_deploy_replace_directories = opt_replace_directories go || opt_replace go
          , conf_debug                      = opt_debug go
          }

getOptions :: IO Options
getOptions = do
    args <- getArgs
    let result = runOptionsParser args
    handleParseResult result

runOptionsParser :: [String] -> ParserResult Options
runOptionsParser strs = execParserPure prefs optionsParser strs
  where
    prefs = ParserPrefs
      { prefMultiSuffix = "SPARK"  -- metavar suffix for multiple options
      , prefDisambiguate = True    -- automatically disambiguate abbreviations (default: False)
      , prefShowHelpOnError = True -- always show help text on parse errors (default: False)
#if MIN_VERSION_optparse_applicative(0,13,0)
      , prefShowHelpOnEmpty = True -- always show help text on empty values (default: False)
#endif
      , prefBacktrack = True       -- backtrack to parent parser when a subcommand fails (default: True)
      , prefColumns = 80           -- number of columns in the terminal, used to format the help page (default: 80)
      }

optionsParser :: ParserInfo Options
optionsParser = info (helper <*> parseOptions) help
  where
    help = fullDesc <> progDesc description
    description = "Super User Spark, author: Tom Sydney Kerckhove"

parseOptions :: Parser Options
parseOptions = (,) <$> parseCommand <*> parseGlobalOptions

parseCommand :: Parser Dispatch
parseCommand = hsubparser $ mconcat
    [ command "parse"   parseParse
    , command "compile" parseCompile
    , command "check"   parseCheck
    , command "deploy"  parseDeploy
    ]

parseParse :: ParserInfo Dispatch
parseParse = info parser modifier
  where
    parser = DispatchParse <$> strArgument (metavar "FILE" <> help "the file to parse")
    modifier = fullDesc
            <> progDesc "Parse a spark file and check for syntactic errors."

parseCompile :: ParserInfo Dispatch
parseCompile = info parser modifier
  where
    parser = DispatchCompile
      <$> argument auto
        (metavar "CARD" <> help "the card file to compile")
    modifier = fullDesc
            <> progDesc "Compile a spark card."

parseCheck :: ParserInfo Dispatch
parseCheck = info parser modifier
  where
    parser = DispatchCheck
      <$> argument auto
         (metavar "CARD" <> help "the card to check")
    modifier = fullDesc
            <> progDesc "Check the deployment of a spark card."

parseDeploy :: ParserInfo Dispatch
parseDeploy = info parser modifier
  where
    parser = DispatchDeploy
      <$> argument auto
        (metavar "CARD" <> help "the card to deploy") -- TODO more help
    modifier = fullDesc
            <> progDesc "Deploy a spark card."


parseGlobalOptions :: Parser GlobalOptions
parseGlobalOptions = GlobalOptions
    <$> option (Just <$> str)
      ( long "output"
        <> short 'o'
        <> value Nothing
        <> metavar "FILE"
        <> help "The output file for compilation" )
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
    <*> switch
      ( long "replace-links"
        <> help "Replace links at deploy destinations." )
    <*> switch
      ( long "replace-files"
        <> help "Replace existing files at deploy destinations." )
    <*> switch
      ( long "replace-Directories"
        <> help "Replace existing directories at deploy destinations." )
    <*> switch
      ( long "replace-all"
        <> short 'r'
        <> help "Equivalent to --replace-files --replace-directories --replace-links" )
    <*> switch
      ( long "debug"
        <> help "Show al debug information." )
