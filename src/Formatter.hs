module Formatter where

import           Data.List  (intersperse)
import           Data.Maybe (catMaybes)

import           Constants
import           Types

formatCards :: [Card] -> Sparker String
formatCards cs = do
    initial <- initialState
    (_, res) <- runSparkFormatter initial (cards cs)
    return res

initialState :: Sparker FormatterState
initialState = return $ FormatterState {
        state_current_indent = 0
    ,   state_longest_src = 0
    ,   state_newline_before_deploy = True
    }


cards :: [Card] -> SparkFormatter ()
cards cs = onLines card cs

delimited :: (a -> SparkFormatter ()) -> [a] -> SparkFormatter ()
delimited thingFormatter things = do
    let allThings = map thingFormatter things
    sequence_ $ intersperse delimiter allThings

onLines :: (a -> SparkFormatter ()) -> [a] -> SparkFormatter ()
onLines thingFormatter things = do
    let allThings = map thingFormatter things
    sequence_ $ intersperse newline allThings


card :: Card -> SparkFormatter ()
card (Card name _ ds) = do
    string keywordCard
    space
    string name
    space
    declaration $ Block ds

braces :: SparkFormatter () -> SparkFormatter ()
braces f = do
    modify (\s -> s {state_newline_before_deploy = True})

    string "{"
    newline
    indented $ do
        newline
        f
        newline
    newline
    string "}"

string :: String -> SparkFormatter ()
string s = tell s

interspersed :: [String] -> String -> SparkFormatter ()
interspersed [] _ = return ()
interspersed [s] _ = string s
interspersed (s:ss) i = do
    string s
    string i
    spaced ss


spaced :: [String] -> SparkFormatter ()
spaced strs = interspersed strs " "

space :: SparkFormatter ()
space = onlyIf (not . conf_format_oneLine) $ string " "


newline :: SparkFormatter ()
newline = do
    onlyIf (not . conf_format_oneLine) $ string "\n"
    ci <- gets state_current_indent
    string $ replicate ci ' '

delimiter :: SparkFormatter ()
delimiter = do
    oneLine <- asks conf_format_oneLine
    if oneLine
    then string ";"
    else newline

indented :: SparkFormatter () -> SparkFormatter ()
indented func = do
    ind <- asks conf_format_indent
    indent ind
    func
    indent (-ind)

indent :: Int -> SparkFormatter ()
indent c = do
    ci <- gets state_current_indent
    modify (\s -> s {state_current_indent = ci + c})

declarations :: [Declaration] -> SparkFormatter ()
declarations = delimited declaration

declaration :: Declaration -> SparkFormatter ()
declaration (SparkOff cr) = do
    string keywordSpark
    space
    cardReference cr
declaration (Deploy src dst k) = do
    nbf <- gets state_newline_before_deploy
    if nbf
    then newline
    else return ()
    quoted src
    ls <- gets state_longest_src
    onlyIf conf_format_lineUp $ string $ replicate (ls - length src) ' '
    string " "
    mkind k
    string " "
    quoted dst
    modify (\s -> s {state_newline_before_deploy = False})
declaration (IntoDir dir) = do
    string keywordInto
    space
    string dir
declaration (OutofDir dir) = do
    string keywordOutof
    space
    string dir
declaration (DeployKindOverride k) = do
    string keywordKindOverride
    space
    case k of
        CopyDeployment -> string keywordCopy
        LinkDeployment -> string keywordLink
declaration (Block ds) = do
    ls <- gets state_longest_src
    let m = maximum $ map srcLen ds
    modify (\s -> s {state_longest_src = m} )
    braces $ declarations ds
    modify (\s -> s {state_longest_src = ls} )
    onlyIf conf_format_trailingNewline newline
  where
    srcLen (Deploy src _ _) = length src
    srcLen _ = 0
declaration (Alternatives ds) = do
    string keywordAlternatives
    space
    spaced ds

quoted :: String -> SparkFormatter ()
quoted str = do
    alwaysQuote <- asks conf_format_alwaysQuote
    if needsQuoting str || alwaysQuote
    then do
        string "\""
        string str
        string "\""
    else do
        string str

needsQuoting :: String -> Bool
needsQuoting str = any (`elem` lineDelimiter ++ whitespaceChars ++ bracesChars) str

onlyIf :: (SparkConfig -> Bool) -> SparkFormatter () -> SparkFormatter ()
onlyIf conf func = do
    b <- asks conf
    if b
    then func
    else return ()

kind :: DeploymentKind -> SparkFormatter ()
kind LinkDeployment = string linkKindSymbol
kind CopyDeployment = string copyKindSymbol

mkind :: Maybe DeploymentKind -> SparkFormatter ()
mkind (Just k) = kind k
mkind Nothing = do
    onlyIf conf_format_lineUp space
    string unspecifiedKindSymbol

cardReference :: CardReference -> SparkFormatter ()
cardReference (CardRepo crr) = cardRepoReference crr
cardReference (CardFile cfr) = cardFileReference cfr
cardReference (CardName cnr) = cardNameReference cnr

cardRepoReference :: CardRepoReference -> SparkFormatter ()
cardRepoReference (CardRepoReference repo mb mfr) = do
    string keywordGit
    string " "
    string $ show repo
    case mb of
        Nothing -> return ()
        Just b -> do
            string branchDelimiter
            string b
    case mfr of
        Nothing -> return ()
        Just (CardFileReference fp mnr) -> do
            string " "
            string fp
            case mnr of
                Nothing -> return ()
                Just (CardNameReference cn) -> do
                    string " "
                    string cn

cardFileReference :: CardFileReference -> SparkFormatter ()
cardFileReference (CardFileReference fp mnr) = do
    string keywordFile
    string " "
    string fp
    case mnr of
        Nothing -> return ()
        Just (CardNameReference cn) -> do
            string " "
            string cn

cardNameReference :: CardNameReference -> SparkFormatter ()
cardNameReference (CardNameReference name) = do
    string keywordCard
    string " "
    string name


srcLen :: Deployment -> [Int]
srcLen (Put srcs _ _) = map length srcs

maximums :: [[Int]] -> [Int]
maximums [[]] = []
maximums lss = if all null lss
    then []
    else (maximum $ map ahead lss):(maximums $ map atail lss)
  where
    ahead [] = 0
    ahead (l:_) = l

    atail [] = []
    atail (_:ls) = ls

formatDeployments :: [Deployment] -> String
formatDeployments ds = unlines $ map (formatDeployment lens) ds
  where lens = maximums $ map srcLen ds

formatDeployment :: [Int] -> Deployment -> String
formatDeployment ms (Put srcs dst k) = unwords $
    [
        padded ms srcs
    ,   kindSymbol k
    ,   dst
    ]
  where
    kindSymbol LinkDeployment = linkKindSymbol
    kindSymbol CopyDeployment = copyKindSymbol

    padded :: [Int] -> [FilePath] -> String
    padded [] [] = []
    padded (m:r) [] = replicate m ' ' ++ padded r []
    padded [] _ = []
    padded (m:r) (s:ss) = s ++ replicate (m - length s) ' ' ++ " " ++ padded r ss

formatPreDeployments :: [(Deployment, PreDeployment)] -> String
formatPreDeployments pdps
    = if null output then "Deployment is done already\n" else unlines output
    where output = catMaybes $ map formatPreDeployment pdps

{-
formatPreDeployments :: [(Deployment, PreDeployment)] -> String
formatPreDeployments ds = unlines $ zipStrs dests $ map (": " ++) ms
  where
    ms = map formatPreDeployment predeps

    dests = map (deployment_dst . fst) ds
    predeps = map snd ds
-}

formatPostDeployments :: [(Deployment, Maybe String)] -> String
formatPostDeployments ds = unlines $ zipStrs dests $ map (": " ++) ms
  where
    ms = map mstr predeps

    mstr Nothing = "done"
    mstr (Just err) = err

    dests = map (deployment_dst . fst) ds
    predeps = map snd ds

formatPreDeployment :: (Deployment, PreDeployment) -> Maybe String
formatPreDeployment (d, (Ready _ _ _)) = Just $ deployment_dst d ++ ": " ++ "ready to deploy"
formatPreDeployment (_, AlreadyDone) = Nothing
formatPreDeployment (d, (Error str)) = Just $ deployment_dst d ++ ": " ++ unwords ["Error:", str]



zipStrs :: [String] -> [String] -> [String]
zipStrs [] [] = []
zipStrs [] ss = ss
zipStrs ss [] = ss
zipStrs (s:ss) (t:ts) = (s++t):(zipStrs ss ts)





