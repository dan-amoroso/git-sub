{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Control.Monad
import           Data.ByteString.Lazy      as B hiding (intercalate, putStrLn,
                                                 readFile)
import           Data.List                 hiding (intercalate)
import           Data.Maybe
import           Data.Monoid
import           Data.Text                 as T
import           Data.Text.Encoding        as T.Encoding
import           Data.Text.IO              (putStrLn)
import           Data.Traversable
import           Filesystem.Path.CurrentOS as Path (FilePath, encode,
                                                    encodeString, fromText,
                                                    toText, valid)
import           Lib                       (parseSubmodules)
import           Options.Applicative
import           Prelude                   hiding (FilePath, putStrLn)
import           System.FilePath.Posix     (dropTrailingPathSeparator)
import           System.Process            (readCreateProcess, shell)

data Command
  = List
  | Add { url :: String, path :: FilePath }
  | Remove { path :: FilePath }
  | Move { from :: FilePath, to :: FilePath }
  deriving (Show, Eq)

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

showHelpOnErrorExecParser = customExecParser (prefs showHelpOnError)

parseCommand :: Parser Command
parseCommand = subparser $ mempty
  <> command "list"
       (info
         (helper <*> parseListCommand)
         (fullDesc <> progDesc "list submodules"))
  <> command "add"
       (info
         (helper <*> parseAddCommand)
         (fullDesc <> progDesc "add a submodule"))
  <> command "rm"
       (info
         (helper <*> parseRemoveCommand)
         (fullDesc <> progDesc "remove a submodule"))
  <> command "mv"
       (info
         (helper <*> parseMoveCommand)
         (fullDesc <> progDesc "move a submodule"))

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
    Left err         -> putStrLn ".gitmodules not found."
    Right submodules -> putStrLn . intercalate "\n" $ T.pack . show <$> submodules

runAdd :: String -> FilePath -> IO ()
runAdd url path =
  if valid path
    then
      do let gitCmd = shell $ "git submodule add " ++ url ++ " " ++ encodeString path
         result <- readCreateProcess gitCmd ""
         putStrLn $ T.pack result
    else putStrLn "invalid path argument"

gitRmCached = "git rm --cached "
gitRmGitmodulesEntry = "git config -f .gitmodules --remove-section submodule."
gitRmGitConfigEntry = "git config -f .git/config --remove-section submodule."

runRemove :: FilePath -> IO ()
runRemove path =
  let textPath = case toText path of
                   Right text -> text
                   Left err   -> err
      cleanPath = if T.last textPath == '/'
                    then T.dropEnd 1 textPath
                    else textPath
      removeCached = shell . T.unpack $ "git rm --cached " <> cleanPath
      removeGitmodulesEntry = shell . T.unpack $ gitRmGitmodulesEntry <> cleanPath
      removeGitConfigEntry = shell . T.unpack$ gitRmGitConfigEntry <> cleanPath
   in do
     readCreateProcess removeCached ""
     readCreateProcess removeGitmodulesEntry ""
     readCreateProcess removeGitConfigEntry ""
     readCreateProcess (shell "git add -u") ""
     putStrLn $ "Submodules entries for " <> T.pack (encodeString path) <> " were deleted."
     putStrLn "The changes have been staged for you."
     putStrLn "Please commit the changes to complete the submodule removal."

runMove :: FilePath -> FilePath -> IO ()
runMove from to = putStrLn "not implemented yet"

parseListCommand :: Parser Command
parseListCommand = pure List

parseAddCommand :: Parser Command
parseAddCommand = Add <$> urlParser <*> pathParser

parseRemoveCommand :: Parser Command
parseRemoveCommand = fmap Remove pathParser

parseMoveCommand :: Parser Command
parseMoveCommand = Move <$> pathParser <*> pathParser

urlParser :: Parser String
urlParser = argument str (metavar "URL" <> help "url of the submodule repository")

pathParser :: Parser FilePath
pathParser =
  argument (str >>= readPath)
    (metavar "PATH" <> help "path to the submodule")

readPath :: String -> ReadM FilePath
readPath s = do
  let path = Path.fromText (T.pack s)
  if Path.valid path
    then return path
    else readerError ("invalid path: " ++ show path)
