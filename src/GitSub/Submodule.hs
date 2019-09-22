module GitSub.Submodule
    ( Submodule
    , parseSubmodule
    , parseSubmodules
    , showPath
    ) where

import           Control.Applicative ()
import           Text.Parsec         (ParseError, Parsec, between, many, noneOf,
                                      parse, spaces, string)

newtype Name = Name String deriving (Eq, Show)
newtype Path = Path String deriving (Eq, Show)
newtype Url = Url String deriving (Eq, Show)

data Submodule = Submodule Name Path Url deriving (Eq, Show)

showPath :: Submodule -> String
showPath (Submodule _ (Path path) _) = path

parseSubmodule :: Parsec String () Submodule
parseSubmodule = do
  name <- between subOpen subClose subName
  spaces
  path <- between pathOpen spaces text
  url <- between urlOpen spaces text
  return $ Submodule (Name name)(Path path) (Url url)
  where
    subOpen = string "[submodule \""
    subClose = string "\"]"
    subName = many $ noneOf "\""
    pathOpen = string "path = "
    urlOpen = string "url = "
    text = many $ noneOf " \n\t"

parseSubmodules :: String -> Either ParseError [Submodule]
parseSubmodules = parse (many parseSubmodule) ".gitmodules"
