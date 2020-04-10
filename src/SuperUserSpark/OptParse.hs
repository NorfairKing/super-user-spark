module SuperUserSpark.OptParse
    ( getDispatch
    , Dispatch(..)
    ) where

import Import

import Options.Applicative.Types
import Options.Applicative
import System.Environment (getArgs)

import SuperUserSpark.OptParse.Types

getDispatch :: IO Dispatch
getDispatch = do
    args <- getArgs
    let result = runOptionsParser args
    handleParseResult result

runOptionsParser :: [String] -> ParserResult Dispatch
runOptionsParser strs = execParserPure prefs_ optionsParser strs
  where
    prefs_ =
        defaultPrefs
        { prefMultiSuffix = ""
        , prefDisambiguate = True
        , prefShowHelpOnError = True
        , prefShowHelpOnEmpty = True
        }

optionsParser :: ParserInfo Dispatch
optionsParser =
    info
        (helper <*> parseDispatch)
        (fullDesc <> progDesc "Super User Spark, author: Tom Sydney Kerckhove")

parseDispatch :: Parser Dispatch
parseDispatch =
    hsubparser $
    mconcat
        [ command "parse" parseParse
        , command "compile" parseCompile
        , command "bake" parseBake
        , command "diagnose" parseDiagnose
        , command "check" parseCheck
        , command "deploy" parseDeploy
        ]

parseParse :: ParserInfo Dispatch
parseParse = info parser modifier
  where
    parser = DispatchParse <$> parseParseArgs
    modifier =
        fullDesc <>
        progDesc "Parse a spark file and check for syntactic errors."

parseParseArgs :: Parser ParseArgs
parseParseArgs =
    ParseArgs <$>
    strArgument (mconcat [metavar "FILE", help "the file to parse"])

parseCompile :: ParserInfo Dispatch
parseCompile = info parser modifier
  where
    parser = DispatchCompile <$> parseCompileArgs
    modifier = fullDesc <> progDesc "Compile a spark card."

parseCompileArgs :: Parser CompileArgs
parseCompileArgs =
    CompileArgs <$>
    strArgument (metavar "CARDREF" <> help "the card file to compile") <*>
    option
        (Just <$> str)
        (mconcat
             [ long "output"
             , short 'o'
             , value Nothing
             , metavar "FILE"
             , help "The output file for compilation"
             ]) <*>
    parseCompileFlags

parseCompileFlags :: Parser CompileFlags
parseCompileFlags =
    CompileFlags <$>
    option
        (Just <$> str)
        (mconcat
             [ long "kind"
             , short 'k'
             , value Nothing
             , metavar "KIND"
             , help
                   "The kind specification for unspecified deployments (default: link)"
             ]) <*>
    option
        (Just <$> str)
        (mconcat
             [ long "override"
             , short 'O'
             , value Nothing
             , metavar "KIND"
             , help "Override every deployment to be of the given kind"
             ])

parseBake :: ParserInfo Dispatch
parseBake = info parser modifier
  where
    parser = DispatchBake <$> parseBakeArgs
    modifier = fullDesc <> progDesc "Bake the raw deployment of a spark card."

parseBakeArgs :: Parser BakeArgs
parseBakeArgs =
    BakeArgs <$> strArgument (metavar "CARDREF" <> help "the card to bake") <*>
    parseBakeFlags

parseBakeFlags :: Parser BakeFlags
parseBakeFlags = BakeFlags <$> parseCompileFlags

parseDiagnose :: ParserInfo Dispatch
parseDiagnose = info parser modifier
  where
    parser = DispatchDiagnose <$> parseDiagnoseArgs
    modifier =
        fullDesc <> progDesc "Diagnose the baked deployment of a spark card."

parseDiagnoseArgs :: Parser DiagnoseArgs
parseDiagnoseArgs =
    DiagnoseArgs <$>
    strArgument (metavar "CARDREF" <> help "the card to diagnose") <*>
    parseDiagnoseFlags

parseDiagnoseFlags :: Parser DiagnoseFlags
parseDiagnoseFlags = DiagnoseFlags <$> parseBakeFlags

parseCheck :: ParserInfo Dispatch
parseCheck = info parser modifier
  where
    parser = DispatchCheck <$> parseCheckArgs
    modifier = fullDesc <> progDesc "Check the deployment of a spark card."

parseCheckArgs :: Parser CheckArgs
parseCheckArgs =
    CheckArgs <$> strArgument (metavar "CARDREF" <> help "the card to check") <*>
    parseCheckFlags

parseCheckFlags :: Parser CheckFlags
parseCheckFlags = CheckFlags <$> parseDiagnoseFlags

parseDeploy :: ParserInfo Dispatch
parseDeploy = info parser modifier
  where
    parser = DispatchDeploy <$> parseDeployArgs
    modifier = fullDesc <> progDesc "Deploy a spark card."

parseDeployArgs :: Parser DeployArgs
parseDeployArgs =
    DeployArgs <$> strArgument (metavar "CARDREF" <> help "the card to deploy") <*>
    parseDeployFlags

parseDeployFlags :: Parser DeployFlags
parseDeployFlags =
    DeployFlags <$>
    switch
        (mconcat
             [ long "replace-links"
             , help "Replace links at deploy destinations."
             ]) <*>
    switch
        (mconcat
             [ long "replace-files"
             , help "Replace existing files at deploy destinations."
             ]) <*>
    switch
        (mconcat
             [ long "replace-Directories"
             , help "Replace existing directories at deploy destinations."
             ]) <*>
    switch
        (mconcat
             [ long "replace-all"
             , short 'r'
             , help
                   "Equivalent to --replace-files --replace-directories --replace-links"
             ]) <*>
    parseCheckFlags
