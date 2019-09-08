module Lib
    ( Submodule
    , parseSubmodule
    , parseSubmodules
    ) where

import           Control.Applicative hiding (many)
import           Data.List           (intercalate)
import           Text.Format
import           Text.Parsec         (Parsec, between, many, noneOf, parse,
                                      spaces, string, (<?>))

type Name = String
type Path = String
type Url = String

data Submodule = Submodule Name Path Url deriving (Eq)

instance Show Submodule where
  show (Submodule _ path _) = path

parseSubmodule :: Parsec String () Submodule
parseSubmodule = do
  name <- between subOpen subClose subName
  spaces
  path <- between pathOpen spaces text
  url <- between urlOpen spaces text
  return $ Submodule name path url
  where
    subOpen = string "[submodule \""
    subClose = string "\"]"
    subName = many $ noneOf "\""
    pathOpen = string "path = "
    urlOpen = string "url = "
    text = many $ noneOf " \n\t"

parseSubmodules = parse (many parseSubmodule) ".gitmodules"

writeSubmodule :: Submodule -> String
writeSubmodule (Submodule name path url) =
  format "[submodule \"{0}\"]\n\tpath = {1}\n\turl = {2}\n" [name, path, url]

someFunc :: IO ()
someFunc = do
  subModString <- readFile ".gitmodules"
  let subModule = parse parseSubmodule ".gitmodules" subModString
  putStrLn $ show subModule
  let subModules = parse (many parseSubmodule) ".gitmodules" subModString
  putStrLn $ show subModules
  putStrLn subModString
  case subModules of
    Left _           -> putStrLn "error!"
    Right submodules -> putStrLn $ concat $ fmap writeSubmodule submodules
