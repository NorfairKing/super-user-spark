{-# LANGUAGE RecordWildCards #-}

module SuperUserSpark.Parser where

import Import

import Control.Exception (try)

import SuperUserSpark.Language.Types
import SuperUserSpark.OptParse.Types
import SuperUserSpark.Parser.Internal
import SuperUserSpark.Parser.Types
import SuperUserSpark.Utils

parseFromArgs :: ParseArgs -> IO ()
parseFromArgs pa = do
    errOrAss <- parseAssignment pa
    case errOrAss of
        Left err -> die $ unwords ["Unable to build parse assignment:", err]
        Right ass -> parse ass

parseAssignment :: ParseArgs -> IO (Either String ParseAssignment)
parseAssignment ParseArgs {..} =
    ParseAssignment <$$>
    ((left (show :: PathParseException -> String)) <$>
     try (resolveFile' parseFilePath))

parse :: ParseAssignment -> IO ()
parse ParseAssignment {..} = do
    errOrFile <- parseFile fileToParse
    case errOrFile of
        Left err -> die $ formatParseError err
        Right _ -> pure ()

formatParseError :: ParseError -> String
formatParseError (ParseError pe) = show pe

parseFile :: Path Abs File -> IO (Either ParseError SparkFile)
parseFile file = (left ParseError . parseCardFile file) <$> readFile file
