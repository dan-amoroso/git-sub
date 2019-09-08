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
--import qualified System.Console.ANSI as ANSI
import           Filesystem.Path.CurrentOS as Path
import           Options.Applicative
import           Prelude                   hiding (FilePath, putStrLn)

smDescription = "utility to manage submodules in a git repository"
smHeader = "git-sub: git submodules made easy"

data Command
  = List
  | Add { url :: String, path :: FilePath }
  | Remove { path :: FilePath }
  | Move { from :: FilePath, to :: FilePath }
  deriving (Show, Eq)

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
    (value "./" <> metavar "PATH" <> help ("path to the submodule"))

readPath :: String -> ReadM FilePath
readPath s = do
  let path = Path.fromText (T.pack s)
  if Path.valid path
    then return path
    else readerError ("invalid path: " ++ (show path))

runList :: IO ()
runList = do
  submodulesStr <- readFile ".gitmodules"
  let submodules = parseSubmodules submodulesStr
  case submodules of
    Left err         -> putStrLn $ (T.pack . show) err
    Right submodules -> do
      -- putStrLn "\nsubmodules in the current repository :"
      putStrLn . intercalate "\n" $ T.pack . show <$> submodules

run :: Command -> IO ()
run command = case command of
  List         -> runList
  Add url path -> undefined
  Remove path  -> undefined
  Move from to -> undefined


showHelpOnErrorExecParser = customExecParser (prefs showHelpOnError)

main :: IO ()
main = do
  command <- showHelpOnErrorExecParser (info
    (helper <*> parseCommand)
    (fullDesc <> progDesc smDescription <> header smHeader))
  run command
