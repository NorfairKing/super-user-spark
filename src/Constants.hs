module Constants where

keywordCard :: String
keywordCard = "card"

keywordSpark :: String
keywordSpark = "spark"

keywordGit :: String
keywordGit = "git"

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

linkKindSymbol :: String
linkKindSymbol = "l->"

copyKindSymbol :: String
copyKindSymbol = "c->"

unspecifiedKindSymbol :: String
unspecifiedKindSymbol = "->"

bracesChars :: [Char]
bracesChars = ['{','}']

linespaceChars :: [Char]
linespaceChars = [' ', '\t']

endOfLineChars :: [Char]
endOfLineChars = ['\n','\r']

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
