{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE RecordWildCards   #-}

module CLI
  ( processLockFile
  , readOptions
  ) where

import           Data.Aeson  (eitherDecodeFileStrict)
import           Nix         hiding (Options, defaultOptions)
import           Nix.TH      (nix)
import           Node2nixHs  (fromPackageLock)
import           NodeEnv     (nodeEnv)
import           PackageLock (PackageLock)

processLockFile :: Options -> IO ()
processLockFile options = do
  let Paths {.. } = paths options
  saveResult   packagesFile  =<< readPackageLock
  writeNixExpr compositionFile (composition options)
  writeNixExpr nodeEnvFile     nodeEnv

readPackageLock :: IO PackageLock
readPackageLock =
  let handleError s =
        die $ "Error while parsing the package-lock.json file: '" <> s <> "'"
   in either handleError pure =<< eitherDecodeFileStrict "./package-lock.json"

saveResult :: FilePath -> PackageLock -> IO ()
saveResult file = writeNixExpr file. fromPackageLock

writeNixExpr :: FilePath -> NExpr -> IO ()
writeNixExpr file expr =
  let prefix = "# This file has been generated by node2nix-hs 0.0.1. Do not edit!\n\n"
   in writeFile file (prefix ++ show (prettyNix expr))

data Options =
  Options
    { paths         :: Paths
    , nodeJSVersion :: NodeJSVersion
    }

data Paths =
  Paths
    { compositionFile :: FilePath
    , packagesFile    :: FilePath
    , nodeEnvFile     :: FilePath
    }

defaultPaths :: Paths
defaultPaths =
  Paths
    {
      -- used to be "default.nix" in original node2nix
      compositionFile = "node-dependencies.nix"
    , packagesFile    = "node-packages.nix"
    , nodeEnvFile     = "node-env.nix"
    }

data NodeJSVersion
  = NodeJS16
  | NodeJS18
  | NodeJS20

toNixPackage :: NodeJSVersion -> Text
toNixPackage = \case
  NodeJS16 -> "nodejs-16_x"
  NodeJS18 -> "nodejs-18_x"
  NodeJS20 -> "nodejs-20_x"

-- FIXME parse arguments
readOptions :: IO Options
readOptions = pure (Options defaultPaths NodeJS18)

composition :: Options -> NExpr
composition Options {..} =
  let nixpkgs = [nix| import <nixpkgs> { inherit system; } |]
      defaultNodePackage = "pkgs" @. toNixPackage nodeJSVersion
      params =
        mkParamSet
          [ ("pkgs"   , Just nixpkgs)
          , ("system" , Just [nix| builtins.currentSystem |])
          , ("nodejs" , Just defaultNodePackage)
          ]

      -- FIXME use callPackage instead
      nodeEnvArgs =
        [nix|
          {
            inherit (pkgs) stdenv lib python2 runCommand writeTextFile writeShellScript;
            inherit pkgs nodejs;
            libtool = if pkgs.stdenv.isDarwin then pkgs.darwin.cctools else null;
          }
        |]

      -- FIXME use callPackage instead
      nodePackagesArgs =
        [nix|
          {
            inherit (pkgs) fetchurl nix-gitignore stdenv lib fetchgit;
            inherit nodeEnv;
          }
        |]
      body =
        mkLets [ "nodeEnv" $= "import" @@ mkPath False (nodeEnvFile paths) @@ nodeEnvArgs ]
          $ "import" @@ mkPath False (packagesFile paths) @@ nodePackagesArgs
   in mkFunction params body
