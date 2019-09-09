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
import           Lib                       (parseSubmodules)
import           System.Process            (readCreateProcess, shell)
--import qualified System.Console.ANSI as ANSI
import           Filesystem.Path.CurrentOS as Path
import           Options.Applicative
import           Prelude                   hiding (FilePath, putStrLn)

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
  <> (command "list"
       (info
         (helper <*> parseListCommand)
         (fullDesc <> progDesc "list submodules")))
  <> (command "add"
       (info
         (helper <*> parseAddCommand)
         (fullDesc <> progDesc "add a submodule")))
  <> (command "rm"
       (info
         (helper <*> parseRemoveCommand)
         (fullDesc <> progDesc "remove a submodule")))
  <> (command "mv"
       (info
         (helper <*> parseMoveCommand)
         (fullDesc <> progDesc "move a submodule")))

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
    Left err         -> putStrLn $ (T.pack . show) err
    Right submodules -> do
      putStrLn . intercalate "\n" $ T.pack . show <$> submodules

runAdd :: String -> FilePath -> IO ()
runAdd url path =
  case (valid path) of
    True -> do
      let gitCmd = shell $ "git submodule add " ++ url ++ " " ++ (encodeString path)
      result <- readCreateProcess gitCmd ""
      putStrLn $ T.pack $ result
    False ->
      putStrLn "invalid path argument"

runRemove :: FilePath -> IO ()
runRemove path = undefined

runMove :: FilePath -> FilePath -> IO ()
runMove from to = undefined

parseListCommand :: Parser Command
parseListCommand = pure(List)

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
    (metavar "PATH" <> help ("path to the submodule"))

readPath :: String -> ReadM FilePath
readPath s = do
  let path = Path.fromText (T.pack s)
  if Path.valid path
    then return path
    else readerError ("invalid path: " ++ (show path))



