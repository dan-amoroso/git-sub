module Main (main) where

import qualified GitSub

main :: IO ()
main = do
  gitSubCommand <- GitSub.parseRootCommand
  GitSub.run gitSubCommand
