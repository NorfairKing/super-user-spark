module Parser (
      parseFile
    ) where

import           Parser.Internal
import           Parser.Types
import           Types

parseFile :: FilePath -> Sparker SparkFile
parseFile file = do
    str <- liftIO $ readFile file
    case parseCardFile file str of
        Left pe -> throwError $ ParseError pe
        Right cs -> return cs
