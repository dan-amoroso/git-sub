module GitSub.CmdParsers where

import           Data.Text                 (pack)
import           Filesystem.Path.CurrentOS as Path (FilePath, fromText, toText,
                                                    valid)
import           Options.Applicative
import           Prelude                   hiding (FilePath)

data Command
  = List
  | Add { url :: String, path :: FilePath }
  | Remove { path :: FilePath }
  | Move { from :: FilePath, to :: FilePath }
  deriving (Show, Eq)

showHelpOnErrorExecParser = customExecParser (prefs showHelpOnError)

parseCommand :: Parser Command
parseCommand = subparser $ mempty
  <> command "ls"
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

parseListCommand :: Parser Command
parseListCommand = pure List

parseAddCommand :: Parser Command
parseAddCommand = Add <$> urlParser <*> pathParser

parseRemoveCommand :: Parser Command
parseRemoveCommand = Remove <$> pathParser

parseMoveCommand :: Parser Command
parseMoveCommand = Move <$> pathParser <*> pathParser

urlParser :: Parser String
urlParser = argument str (metavar "URL" <> help "url of the submodule repository")

pathParser :: Parser FilePath
pathParser =
  argument (str >>= readPath)
    (metavar "PATH" <> help "file-path to the submodule")

readPath :: String -> ReadM FilePath
readPath s = do
  let path = Path.fromText (pack s)
  if Path.valid path
    then return path
    else readerError ("invalid path: " ++ show path)

