module Constants where

keywordCard :: String
keywordCard = "card"

keywordSpark :: String
keywordSpark = "spark"

keywordFile :: String
keywordFile = "file"

keywordInto :: String
keywordInto = "into"

keywordOutof :: String
keywordOutof = "outof"

keywordKindOverride :: String
keywordKindOverride = "kind"

keywordLink :: String
keywordLink = "link"

keywordCopy :: String
keywordCopy = "copy"

keywordDecrypt :: String
keywordDecrypt = "decrypt"

keywordAlternatives :: String
keywordAlternatives = "alternatives"

linkKindSymbol :: String
linkKindSymbol = "l->"

copyKindSymbol :: String
copyKindSymbol = "c->"

decryptKindSymbol :: String
decryptKindSymbol = "d->"

unspecifiedKindSymbol :: String
unspecifiedKindSymbol = "->"

bracesChars :: [Char]
bracesChars = ['{', '}']

linespaceChars :: [Char]
linespaceChars = [' ', '\t']

endOfLineChars :: [Char]
endOfLineChars = ['\n', '\r']

whitespaceChars :: [Char]
whitespaceChars = linespaceChars ++ endOfLineChars

lineDelimiter :: String
lineDelimiter = ";"

branchDelimiter :: String
branchDelimiter = ":"

quotesChar :: Char
quotesChar = '"'

lineCommentStr :: String
lineCommentStr = "#"

blockCommentStrs :: (String, String)
blockCommentStrs = ("[[", "]]")

sparkExtension :: String
sparkExtension = "sus"
