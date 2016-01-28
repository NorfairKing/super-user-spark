module Parser (
      parseCardFileReference
    , parseFile
    ) where


import           Data.List       (find)

import           Parser.Internal
import           Parser.Types
import           Types


parseCardFileReference :: CardFileReference -> Sparker SparkFile
parseCardFileReference (CardFileReference fp mnr) = do
    css <- parseFile fp
    case mnr of
        Nothing -> return css
        Just (CardNameReference cn) ->
            case find (\s -> card_name s == cn) css of
                Nothing -> throwError $ UnpredictedError $ unwords ["Did't find card", "\"" ++ cn ++ "\"", "in", fp]
                Just c  -> return [c]

-- | Only use this in practice (outside tests)
parseFile :: FilePath -> Sparker SparkFile
parseFile file = do
    str <- liftIO $ readFile file
    case parseCardFile file str of
        Left pe -> throwError $ ParseError pe
        Right cs -> return cs
