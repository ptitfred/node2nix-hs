module Main
  ( main
  ) where

import           Node2nixHs (projectName)

main :: IO ()
main = putStrLn ("Tests for " ++ projectName)
