module Parser (
      parseFile
    , resolveUnits
    ) where

import           Compiler.Types
import           Parser.Internal
import           Parser.Types
import           Types

parseFile :: FilePath -> Sparker SparkFile
parseFile file = do
    sf <- liftIO $ parseFileIO file
    case sf of
        Left pe -> throwError $ ParseError pe
        Right cs -> return cs

parseFileIO :: FilePath -> IO (Either ParseError SparkFile)
parseFileIO file = (liftIO $ readFile file) >>= return . parseCardFile file

resolveUnits :: SparkFile -> [CompileUnit]
resolveUnits (SparkFile fp cs) = map (Unit fp) cs
