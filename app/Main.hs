{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Data.List                 hiding (intercalate)
import           Data.Monoid
import           Data.String               (IsString)
import           Data.Text                 as T hiding (intercalate, unlines)
import           Filesystem.Path.CurrentOS as Path (FilePath, encodeString,
                                                    toText, valid)
import           GitSub.CmdParsers
import           GitSub.Submodule          (parseSubmodules)
import           Options.Applicative
import           Prelude                   hiding (FilePath)
import           System.Process            (readCreateProcess, shell)

main :: IO ()
main = do
  command <-
    showHelpOnErrorExecParser
      (info
        (helper <*> parseCommand)
        (fullDesc <> progDesc smDescription <> header smHeader))
  run command

smDescription = "utility to manage submodules in a git repository"
smHeader = "git-sub: git submodules made easy"

run :: Command -> IO ()
run command = case command of
  List         -> runList
  Add url path -> runAdd url path
  Remove path  -> runRemove path
  Move from to -> runMove from to

runList :: IO ()
runList = do
  submodulesStr <- readFile ".gitmodules"
  let submodules = parseSubmodules submodulesStr
  case submodules of
    Left err         -> print ".gitmodules not found."
    Right submodules -> putStrLn . unlines $ show <$> submodules

runAdd :: String -> FilePath -> IO ()
runAdd url path =
  if valid path
    then do
      result <- runShell $ "git submodule add " ++ url ++ " " ++ encodeString path
      print result
    else print "invalid path argument"

gitRmCached = "git rm --cached "
gitRmGitmodulesEntry = "git config -f .gitmodules --remove-section submodule."
gitRmGitConfigEntry = "git config -f .git/config --remove-section submodule."
gitStage = "git add -u"

runRemove :: FilePath -> IO ()
runRemove path = do
  runShell removeCached
  runShell removeGitmodulesEntry
  runShell removeGitConfigEntry
  runShell gitStage
  print $ "Submodules entries for " <> path' <> " were deleted."
  print "The changes have been staged for you."
  print "Please commit the changes to complete the submodule removal."
  where removeCached = "git rm --cached " <> path'
        removeGitmodulesEntry = gitRmGitmodulesEntry <> path'
        removeGitConfigEntry = gitRmGitConfigEntry <> path'
        path' = cleanTxt path

runMove :: FilePath -> FilePath -> IO ()
runMove from to = do
  runShell $ gitMove <> from' <> " " <> to'
  runShell gitStage
  print $ "Submodule moved from " <> from' <> " to " <> to'
  print "The changes have been staged for you."
  print "Please commit the changes to complete the submodule move."
  where gitMove = "git mv "
        from' = cleanTxt from
        to' = cleanTxt to

cleanTxt :: FilePath -> Text
cleanTxt = removeTrailingSlash . toText
  where removeTrailingSlash txtPath = case txtPath of
          Right text -> if T.last text == '/' then T.dropEnd 1 text else text
          Left err   -> err

runShell :: (Show a, IsString a) => a -> IO String
runShell str = readCreateProcess (shell $ show str) ""

