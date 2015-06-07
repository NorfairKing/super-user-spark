module Formatter where

import           Data.List (intersperse)

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
    }

indentation :: Int
indentation = 4

cards :: [Card] -> SparkFormatter ()
cards cs = do
    onLines card cs

onLines :: (a -> SparkFormatter ()) -> [a] -> SparkFormatter ()
onLines thingFormatter things = do
    let all = map  thingFormatter things
    sequence_ $ intersperse newline all


card :: Card -> SparkFormatter ()
card (Card name _ ds) = do
    string keywordCard
    string " "
    string name
    string " "
    declaration $ Block ds
    newline

braces :: SparkFormatter () -> SparkFormatter ()
braces f = do
    modify (\s -> s {state_newline_before_deploy = True})
    string "{"
    newline
    indent 4
    newline
    f
    newline
    indent (-4)
    newline
    string "}"

string :: String -> SparkFormatter ()
string s = tell s

newline :: SparkFormatter ()
newline = do
    string "\n"
    ci <- gets state_current_indent
    string $ replicate ci ' '

indent :: Int -> SparkFormatter ()
indent c = do
    ci <- gets state_current_indent
    modify (\s -> s {state_current_indent = ci + c})

declarations :: [Declaration] -> SparkFormatter ()
declarations = onLines declaration

declaration :: Declaration -> SparkFormatter ()
declaration (SparkOff cr) = do
    string keywordSpark
    string " "
    cardReference cr
declaration (Deploy src dst k) = do
    nbf <- gets state_newline_before_deploy
    if nbf
    then newline
    else return ()
    string src
    ls <- gets state_longest_src
    string $ replicate (ls - length src) ' '
    string " "
    kind k
    string " "
    string dst
    modify (\s -> s {state_newline_before_deploy = False})
declaration (IntoDir dir) = do
    string keywordInto
    string " "
    string dir
declaration (OutofDir dir) = do
    string keywordOutof
    string " "
    string dir
declaration (DeployKindOverride k) = do
    string keywordKindOverride
    string " "
    kind k
declaration (Block ds) = do
    ls <- gets state_longest_src
    let m = maximum $ map srcLen ds
    modify (\s -> s {state_longest_src = m} )
    braces $ declarations ds
    modify (\s -> s {state_longest_src = ls} )
  where
    srcLen (Deploy src dst k) = length src
    srcLen _ = 0

kind :: DeploymentKind -> SparkFormatter ()
kind LinkDeployment = string linkKindSymbol
kind CopyDeployment = string copyKindSymbol
kind UnspecifiedDeployment = string $ ' ':unspecifiedKindSymbol

cardReference :: CardReference -> SparkFormatter ()
cardReference (CardRepo repo mfpmcn) = do
    string keywordGit
    string " "
    string $ show repo
    case mfpmcn of
        Nothing -> return ()
        Just (fp, mcn) -> do
            string " "
            string fp
            case mcn of
                Nothing -> return ()
                Just cn -> do
                    string " "
                    string cn

cardReference (CardFile fp mcn) = do
    string keywordFile
    string " "
    string fp
    case mcn of
        Nothing -> return ()
        Just cn -> do
            string " "
            string cn

cardReference (CardName name) = do
    string keywordCard
    string " "
    string name



formatDeployments :: [Deployment] -> String
formatDeployments ds = unlines $ map (formatDeployment (maximum $ map srcLen ds)) ds
  where
    srcLen (Link src _) = length src
    srcLen (Copy src _) = length src
    srclen _ = 0

formatDeployment :: Int -> Deployment -> String
formatDeployment n (Link src dst) = unwords [src, replicate (n-length src) ' ', "l->", dst]
formatDeployment n (Copy src dst) = unwords [src, replicate (n-length src) ' ', "c->", dst]

