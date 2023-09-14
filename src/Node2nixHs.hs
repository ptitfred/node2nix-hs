{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE DuplicateRecordFields      #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}

module Node2nixHs
  ( nodePackages
  , fromPackageLock
  , PackageDefinition(..)
  , PackageSource(..)
  , Dependency(..)
  , Source(..)
  , Hash(..)
  ) where

import qualified Data.Map.Strict as M
import           Data.Text       (replace)
import           Nix
import           PackageLock     (Package (..), PackageLock (..),
                                  packageNameFromPackageKey,
                                  selectRecursiveDependencies,
                                  selectRootPackages)
import           Text.Parsec     (anyChar, choice, many1, runParser, string)

data PackageDefinition =
  PackageDefinition
    { packageName  :: Text
    , src          :: FilePath
    , dependencies :: [Dependency]
    }

data Dependency = SimpleDependency    QName
                | RecursiveDependency QName [Dependency]

instance IsString Dependency where
  fromString = SimpleDependency . fromString

newtype QName = QName Text deriving newtype (IsString)

mkQName :: PackageSource -> QName
mkQName PackageSource { packageName, version } =
  QName (packageName <> "-" <> version)

data PackageSource =
  PackageSource
    { packageName :: Text
    , version     :: Text
    , source      :: Source
    }

data Source =
  Source
    { url  :: Text
    , hash :: Hash
    }

data Hash
  = SHA1   Text
  | SHA512 Text

hashFromIntegrity :: Text -> Maybe Hash
hashFromIntegrity =
  let runParsec p = runParser p () ""
      parser =
        choice
          [ string "sha512-" *> (SHA512 . fromString <$> many1 anyChar)
          , string "sha1-"   *> (SHA1   . fromString <$> many1 anyChar)
          ]
   in either (const Nothing) pure . runParsec parser

mkSources :: [PackageSource] -> NExpr
mkSources =
  let mkSingleSourceBinding ps = renderQName (mkQName ps) $= mkPackageSource ps
   in mkNonRecSet . fmap mkSingleSourceBinding

quote :: Text -> Text
quote s = "\"" <> s <> "\""

mkPackageSource :: PackageSource -> NExpr
mkPackageSource PackageSource { .. } =
  mkNonRecSet
    [ "name"        $= mkStr (nixSafeName packageName)
    , "packageName" $= mkStr packageName
    , "version"     $= mkStr version
    , "src"         $= fetchurl source
    ]

nixSafeName :: Text -> Text
nixSafeName
  = replace "@" "_at_"
  . replace "/" "_slash_"

fetchurl :: Source -> NExpr
fetchurl (Source url hash) =
  case hash of
    SHA1   h -> "fetchurl" @@ mkNonRecSet [ "url" $= mkStr url, "sha1"   $= mkStr h ]
    SHA512 h -> "fetchurl" @@ mkNonRecSet [ "url" $= mkStr url, "sha512" $= mkStr h ]

mkArgs :: PackageDefinition -> NExpr
mkArgs PackageDefinition { .. } =
  mkNonRecSet
    [ "name"            $= mkStr (nixSafeName packageName)
    , "packageName"     $= mkStr packageName
    , "src"             $= mkPath False src
    , lookupDependencies dependencies
    , "buildInputs"     $= "globalBuildInputs"
    , "meta"            $= emptySet
    , "production"      $= mkBool True
    , "bypassCache"     $= mkBool True
    , "reconstructLock" $= mkBool False
    ]

lookupDependencies :: [Dependency] -> Binding NExpr
lookupDependencies deps = "dependencies" $= mkList (lookupDependency <$> deps)

lookupDependency :: Dependency -> NExpr
lookupDependency = \case
  SimpleDependency    qn         -> lookupSource qn
  RecursiveDependency qn subDeps -> lookupSource qn $//
    mkNonRecSet [ lookupDependencies subDeps ]

lookupSource :: QName -> NExpr
lookupSource qn = "sources" @. renderQName qn

renderQName :: QName -> Text
renderQName (QName qn) = quote qn

fromPackageLock :: PackageLock -> NExpr
fromPackageLock = uncurry nodePackages . (definitionFromPackageLock &&& sourcesFromPackageLock)

definitionFromPackageLock :: PackageLock -> PackageDefinition
definitionFromPackageLock PackageLock { name, packages, src } =
  let mkDep (depName, Package{ version }) =
        let deps = selectRecursiveDependencies depName packages
            qn = QName (depName <> "-" <> version)
         in if M.null deps
            then SimpleDependency qn
            else RecursiveDependency qn (mkDep <$> selectRootPackages deps)
   in PackageDefinition
        { packageName  = name
        , src
        , dependencies = mkDep <$> selectRootPackages packages
        }

sourcesFromPackageLock :: PackageLock -> [PackageSource]
sourcesFromPackageLock PackageLock { packages } =
  let sources = sortOn deriveKey $ M.elems $ M.mapMaybeWithKey buildSource packages
      deriveKey PackageSource { packageName, version } = packageName <> "-" <> version
      buildSource pk Package{..} = do
        hash <- hashFromIntegrity integrity
        pure $
          PackageSource { packageName = packageNameFromPackageKey pk
                        , version
                        , source = Source { url = resolved, hash }
                        }
   in sources

nodePackages :: PackageDefinition -> [PackageSource] -> NExpr
nodePackages packageDefinition sources =
  let mkRequired n   = (n, Nothing)
      mkOptional n v = (n, Just v)
      topLevelParams =
        mkParamSet
          [ mkRequired "nodeEnv"
          , mkRequired "fetchurl"
          , mkRequired "fetchgit"
          , mkRequired "nix-gitignore"
          , mkRequired "stdenv"
          , mkRequired "lib"
          , mkOptional "globalBuildInputs" emptyList
          ]
      bindings =
        [ "sources" $= mkSources sources
        , "args"    $= mkArgs packageDefinition
        ]
      nodeDependenciesArgs = "lib" @. "overrideExisting" @@ "args" @@ mkNonRecSet
        [ "src" $= "stdenv" @. "mkDerivation" @@ mkNonRecSet
            [ "name" $= "args" @. "name" $+ mkStr "-package-json"
            , "src" $= "nix-gitignore" @. "gitignoreSourcePure" @@ mkList
                [ mkStr "*"
                , mkStr "!package.json"
                , mkStr "!package-lock.json"
                ] @@ "args" @. "src"
            , "dontBuild" $= mkBool True
            , "installPhase" $= mkStr "mkdir -p $out; cp -r ./* $out;"
            ]
        ]
      body =
        mkNonRecSet
          [ inherit [ "sources", "args" ]
          , "tarball"          $= "nodeEnv" @. "buildNodeSourceDist"   @@ "args"
          , "package"          $= "nodeEnv" @. "buildNodePackage"      @@ "args"
          , "shell"            $= "nodeEnv" @. "buildNodeShell"        @@ "args"
          , "nodeDependencies" $= "nodeEnv" @. "buildNodeDependencies" @@ nodeDependenciesArgs
          ]
   in mkFunction topLevelParams (mkLets bindings body)
