module SuperUserSpark.Parser where

import Import

import Control.Monad.Except

import Control.Exception (try)
import SuperUserSpark.Language.Types
import SuperUserSpark.Monad
import SuperUserSpark.Parser.Internal
import Text.Parsec hiding (try)

parseFile :: FilePath -> Sparker SparkFile
parseFile file = do
    esf <- liftIO $ try $ parseFileIO file
    case esf of
        Left ioe -> throwError $ UnpredictedError $ show (ioe :: IOError)
        Right sf -> do
            case sf of
                Left pe -> throwError $ ParseError pe
                Right cs -> return cs

parseFileIO :: FilePath -> IO (Either ParseError SparkFile)
parseFileIO file = (liftIO $ readFile file) >>= return . parseCardFile file
