module Main
  ( main
  ) where

import           CLI

main :: IO ()
main = readOptions >>= processLockFile
