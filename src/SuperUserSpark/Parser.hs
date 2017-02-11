{-# LANGUAGE RecordWildCards #-}

module SuperUserSpark.Parser where

import Import

import Text.Parsec hiding (try)

import SuperUserSpark.Language.Types
import SuperUserSpark.OptParse.Types
import SuperUserSpark.Parser.Internal

parseFromArgs :: ParseArgs -> IO ()
parseFromArgs ParseArgs {..} = do
    errOrDone <- parseFile parseFilePath
    case errOrDone of
        Left err -> die $ formatParseError err
        Right _ -> pure ()

formatParseError :: ParseError -> String
formatParseError = show

parseFile :: FilePath -> IO (Either ParseError SparkFile)
parseFile file = (liftIO $ readFile file) >>= return . parseCardFile file
