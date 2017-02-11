module SuperUserSpark.Arguments where

import Import

import Options.Applicative
import System.Environment (getArgs)

import SuperUserSpark.Config
import SuperUserSpark.Config.Types
import SuperUserSpark.Dispatch.Types
import SuperUserSpark.Types
import SuperUserSpark.Utils

getInstructions :: IO Instructions
getInstructions = do
    mins <- fmap transformOptions getOptions
    case mins of
        Right ins -> return ins
        Left err -> die $ "Failed to parse instructions\n" ++ err

transformOptions :: Options -> Either String Instructions
transformOptions (dispatch, go) = (,) <$> pure dispatch <*> configFromOptions go

configFromOptions :: GlobalOptions -> Either String SparkConfig
configFromOptions go = Right conf
  where
    conf =
        defaultConfig
        { confCompileOutput = optOutput go
        , confCompileKind = optKind go
        , confCompileOverride = optOverride go
        , confDeployReplaceLinks = optReplaceLinks go || optReplace go
        , confDeployReplaceFiles = optReplaceFiles go || optReplace go
        , confDeployReplaceDirectories =
              optReplaceDirectories go || optReplace go
        , confDebug = optDebug go
        }

getOptions :: IO Options
getOptions = do
    args <- getArgs
    let result = runOptionsParser args
    handleParseResult result

runOptionsParser :: [String] -> ParserResult Options
runOptionsParser strs = execParserPure prefs_ optionsParser strs
  where
    prefs_ =
        ParserPrefs
        { prefMultiSuffix = ""
        , prefDisambiguate = True
        , prefShowHelpOnError = True
        , prefShowHelpOnEmpty = True
        , prefBacktrack = True
        , prefColumns = 80
        }

optionsParser :: ParserInfo Options
optionsParser =
    info
        (helper <*> parseOptions)
        (fullDesc <> progDesc "Super User Spark, author: Tom Sydney Kerckhove")

parseOptions :: Parser Options
parseOptions = (,) <$> parseCommand <*> parseGlobalOptions

parseCommand :: Parser Dispatch
parseCommand =
    hsubparser $
    mconcat
        [ command "parse" parseParse
        , command "compile" parseCompile
        , command "check" parseCheck
        , command "deploy" parseDeploy
        ]

parseParse :: ParserInfo Dispatch
parseParse = info parser modifier
  where
    parser =
        DispatchParse <$>
        strArgument (metavar "FILE" <> help "the file to parse")
    modifier =
        fullDesc <>
        progDesc "Parse a spark file and check for syntactic errors."

parseCompile :: ParserInfo Dispatch
parseCompile = info parser modifier
  where
    parser =
        DispatchCompile <$>
        argument auto (metavar "CARD" <> help "the card file to compile")
    modifier = fullDesc <> progDesc "Compile a spark card."

parseCheck :: ParserInfo Dispatch
parseCheck = info parser modifier
  where
    parser =
        DispatchCheck <$>
        argument auto (metavar "CARD" <> help "the card to check")
    modifier = fullDesc <> progDesc "Check the deployment of a spark card."

parseDeploy :: ParserInfo Dispatch
parseDeploy = info parser modifier
  where
    parser =
        DispatchDeploy <$>
        argument
            auto
            (metavar "CARD" <> help "the card to deploy") -- TODO more help
    modifier = fullDesc <> progDesc "Deploy a spark card."

parseGlobalOptions :: Parser GlobalOptions
parseGlobalOptions =
    GlobalOptions <$>
    option
        (Just <$> str)
        (long "output" <> short 'o' <> value Nothing <> metavar "FILE" <>
         help "The output file for compilation") <*>
    option
        (Just <$> auto)
        (long "kind" <> short 'k' <> value Nothing <> metavar "KIND" <>
         help
             "The kind specification for unspecified deployments (default: link)") <*>
    option
        (Just <$> auto)
        (long "override" <> short 'O' <> value Nothing <> metavar "KIND" <>
         help "Override every deployment to be of the given kind") <*>
    switch
        (long "replace-links" <> help "Replace links at deploy destinations.") <*>
    switch
        (long "replace-files" <>
         help "Replace existing files at deploy destinations.") <*>
    switch
        (long "replace-Directories" <>
         help "Replace existing directories at deploy destinations.") <*>
    switch
        (long "replace-all" <> short 'r' <>
         help
             "Equivalent to --replace-files --replace-directories --replace-links") <*>
    switch (long "debug" <> help "Show al debug information.")
