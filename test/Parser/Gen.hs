module Parser.Gen where

import           Test.Hspec
import           Test.QuickCheck

generateNormalCharacter :: Gen Char
generateNormalCharacter = elements $ ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'1']

generateWord :: Gen String
generateWord = listOf1 generateNormalCharacter

generateTab :: Gen Char
generateTab = return '\t'

generateSpace :: Gen Char
generateSpace = return ' '

generateLineFeed :: Gen Char
generateLineFeed= return '\n'

generateCarriageReturn :: Gen Char
generateCarriageReturn= return '\r'

generateLineSpace :: Gen String
generateLineSpace = listOf $ oneof [generateTab, generateSpace]

generateWhiteSpace :: Gen String
generateWhiteSpace = listOf $ oneof [generateTab, generateSpace, generateLineFeed, generateCarriageReturn]

generateWords :: Gen String
generateWords = fmap unwords $ listOf1 generateWord

generateEol :: Gen String
generateEol = elements ["\n", "\r", "\r\n"]

twice :: Gen a -> Gen (a, a)
twice gen = (,) <$> gen <*> gen

trice :: Gen a -> Gen (a, a, a)
trice gen = (,,) <$> gen <*> gen <*> gen

generateCardName :: Gen (String, String)
generateCardName = oneof [generateQuotedIdentifier, generatePlainIdentifier]

generateQuotedIdentifier :: Gen (String, String)
generateQuotedIdentifier = do
    w <- generateWord
    return $ ("\"" ++ w ++ "\"", w)

generatePlainIdentifier :: Gen (String, String)
generatePlainIdentifier = do
    w <- generateWord
    return $ (w, w)

