module CLI.Options
  ( readOptions
  , Options(..)
  , Paths(..)
  , NodeJSVersion(..)
  ) where

import           Options.Applicative

readOptions :: IO Options
readOptions =
  execParser (info (optionsParser <**> helper <**> simpleVersioner "v0.0.1.0") (fullDesc <> progDesc "node2nix alternative implementation supporting nodeJS 18+"))

optionsParser :: Parser Options
optionsParser =
  Options <$> pathsParser
          <*> noCopyNodeEnvParser
          <*> nodeJSVersionParser

pathsParser :: Parser Paths
pathsParser =
  let pathOption s l v h = strOption (short s <> long l <> help h <> value v)
      -- used to be "default.nix" in original node2nix
      compositionOption = pathOption 'c' "composition" "node-dependencies.nix" "Path to a Nix composition expression allowing someone to deploy the generated Nix packages from the command-line (defaults to: default.nix)"
      outputOption      = pathOption 'o' "output"      "node-packages.nix"     "Path to a Nix expression representing a registry of Node.js packages (defaults to: node-packages.nix)"
      nodeEnvOption     = pathOption 'e' "node-env"    "node-env.nix"          "Path to the Nix expression implementing functions that builds NPM packages (defaults to: node-env.nix)"
      packageLockOption = pathOption 'l' "lock"        "./package-lock.json"   "Path to the package-lock.json file that pinpoints the variants of all dependencies"
   in Paths <$> compositionOption
            <*> outputOption
            <*> nodeEnvOption
            <*> packageLockOption

noCopyNodeEnvParser :: Parser Bool
noCopyNodeEnvParser = switch (long "no-copy-node-env" <> help "Do not create a copy of the Nix expression that builds NPM packages")

nodeJSVersionParser :: Parser NodeJSVersion
nodeJSVersionParser =
      flag' NodeJS16 (long "nodejs-16" <> help "Provides all settings to generate expression for usage with Node.js 16.x (default is: nodejs-18_x)")
  <|> flag' NodeJS18 (long "nodejs-18" <> help "Provides all settings to generate expression for usage with Node.js 18.x (default is: nodejs-18_x)")
  <|> flag' NodeJS20 (long "nodejs-20" <> help "Provides all settings to generate expression for usage with Node.js 20.x (default is: nodejs-18_x)")
  <|> pure NodeJS18

data Options =
  Options
    { paths         :: Paths
    , noCopyNodeEnv :: Bool
    , nodeJSVersion :: NodeJSVersion
    }

data NodeJSVersion
  = NodeJS16
  | NodeJS18
  | NodeJS20

data Paths =
  Paths
    { compositionFile :: FilePath
    , packagesFile    :: FilePath
    , nodeEnvFile     :: FilePath
    , packageLockFile :: FilePath
    }

simpleVersioner :: String -- ^ Version string to be shown
                -> Parser (a -> a)
simpleVersioner version =
  infoOption version
    (long "version" <> short 'v' <> help "Show version information")
