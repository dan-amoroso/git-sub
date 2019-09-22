{-# LANGUAGE OverloadedStrings #-}

module GitSub (run, parseRootCommand) where

import           Data.List                 hiding (intercalate)
import           Data.Monoid
import           Data.Text                 as T hiding (intercalate, unlines)
import           Filesystem.Path.CurrentOS as Path (FilePath, encodeString,
                                                    toText, valid)

import           GitSub.CmdParsers
import           GitSub.Submodule          (parseSubmodules, showPath)
import           Options.Applicative
import           Prelude                   hiding (FilePath)
import           System.Process            (readCreateProcess, shell)

run :: Command -> IO ()
run gitSubCommand = case gitSubCommand of
  List                        -> runList
  Add (SubmoduleUrl url) path -> runAdd url path
  Remove path                 -> runRemove path
  Move from to                -> runMove from to

runList :: IO ()
runList = do
  submodulesStr <- readFile ".gitmodules"
  let submodules = parseSubmodules submodulesStr
  case submodules of
    Left _     -> printText ".gitmodules not found."
    Right subs -> putStrLn . unlines $ showPath <$> subs

runAdd :: String -> FilePath -> IO ()
runAdd url path =
  if valid path
    then do
      result <- runShell $ "git submodule add " ++ url ++ " " ++ encodeString path
      print result
    else printText "invalid path argument"

runRemove :: FilePath -> IO ()
runRemove path = do
  runShell removeCached
  runShell removeGitmodulesEntry
  runShell removeGitConfigEntry
  runShell gitStage
  printText $ "Submodules entries for " <> path' <> " were deleted."
  printText "The changes have been staged for you."
  printText "Please commit the changes to complete the submodule removal."
  where removeCached = gitRmCached <> path'
        removeGitmodulesEntry = gitRmGitmodulesEntry <> path'
        removeGitConfigEntry = gitRmGitConfigEntry <> path'
        path' = cleanTxt path

runMove :: FilePath -> FilePath -> IO ()
runMove from to = do
  runShell $ gitMove <> from' <> " " <> to'
  runShell gitStage
  printText $ "Submodule moved from " <> from' <> " to " <> to'
  printText "The changes have been staged for you."
  printText "Please commit the changes to complete the submodule move."
  where gitMove = "git mv "
        from' = cleanTxt from
        to' = cleanTxt to

parseRootCommand :: IO Command
parseRootCommand =
    showHelpOnErrorExecParser
      (info
        (helper <*> parseCommand)
        (fullDesc <> progDesc cliDescription <> header cliHeader))

cliDescription :: String
cliDescription = "utility to manage submodules in a git repository"

cliHeader :: String
cliHeader = "git-sub: git submodules made easy"


gitRmCached :: Text
gitRmCached = "git rm --cached "

gitRmGitmodulesEntry :: Text
gitRmGitmodulesEntry = "git config -f .gitmodules --remove-section submodule."

gitRmGitConfigEntry :: Text
gitRmGitConfigEntry = "git config -f .git/config --remove-section submodule."

gitStage :: Text
gitStage = "git add -u"

cleanTxt :: FilePath -> Text
cleanTxt = removeTrailingSlash . toText
  where removeTrailingSlash txtPath = case txtPath of
          Right text -> if T.last text == '/' then T.dropEnd 1 text else text
          Left err   -> err

runShell :: (Show a) => a -> IO String
runShell strCommand = readCreateProcess (shell $ show strCommand) ""

printText :: Text -> IO ()
printText = print
