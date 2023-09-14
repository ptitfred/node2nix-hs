{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TupleSections              #-}

module PackageLock
  ( loadPackageLock
  , PackageLock(..)
  , Packages
  , PackageKey
  , packageNameFromPackageKey
  , Package(..)
  , parseKey
  , selectRootPackages
  , selectRecursiveDependencies
  ) where

import           Data.Aeson
import           Data.Aeson.Types   (parseMaybe)
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict    as M
import           System.FilePath    (takeDirectory)
import           Text.Parsec        (ParsecT, anyChar, choice, eof, many1,
                                     manyTill, runParser, string, try)

data PackageLock =
  PackageLock
    { name            :: Text
    , lockfileVersion :: Int
    , packages        :: Packages
    , src             :: FilePath
    } deriving stock (Show)

loadPackageLock :: FilePath -> IO (Either String PackageLock)
loadPackageLock file =
  let realise (UnrealisedPackageLock unrealised) = unrealised (takeDirectory file)
   in fmap realise <$> eitherDecodeFileStrict file

newtype UnrealisedPackageLock = UnrealisedPackageLock (FilePath -> PackageLock)

instance FromJSON UnrealisedPackageLock where
  parseJSON = withObject "PackageLock" $ \o -> do
    lockfileVersion <- o .: "lockfileVersion"
    if lockfileVersion == 1
    then fail "lockfileVersion 1 not supported"
    else do
      name     <- o .: "name"
      packages <- interpretKeys <$> o .: "packages"
      pure $ UnrealisedPackageLock (\src -> PackageLock{..})

newtype PackageKey = PackageKey (NonEmpty Text) deriving newtype (Show, Eq, Ord, Hashable, FromJSON)

type Packages = Map PackageKey Package

newtype RawPackage = RawPackage { unRawPackage :: Maybe Package }

instance FromJSON RawPackage where
  parseJSON = pure . RawPackage . parseMaybe parseJSON

interpretKeys :: Map Text RawPackage -> Map PackageKey Package
interpretKeys =
  let decodeKey k package =
        maybe id (uncurry M.insert) ((,) <$> parseKey k <*> unRawPackage package)
   in M.foldrWithKey decodeKey mempty

packageNameFromPackageKey :: PackageKey -> Text
packageNameFromPackageKey (PackageKey keys) = last keys

selectRootPackages :: Packages -> [(Text, Package)]
selectRootPackages =
  let pickRoot (PackageKey k) v = (, v) <$> trySingleton k
      trySingleton = \case
        k :| [] -> Just k
        _       -> Nothing
   in M.elems . M.mapMaybeWithKey pickRoot

selectRecursiveDependencies :: Text -> Packages -> Packages
selectRecursiveDependencies packageName =
  let insertSubDependency (PackageKey k) package packages =
        if [packageName] `NE.isPrefixOf` k && length k > 1
        then foldMap (\k' -> M.insert (PackageKey k') package packages) (nonEmpty (tail k))
        else packages
   in M.foldrWithKey insertSubDependency mempty

parseKey :: Text -> Maybe PackageKey
parseKey =
  let runParsec p = either (const Nothing) pure . runParser p () ""
   in (fmap PackageKey . nonEmpty) <=< runParsec packageKeyParser

packageKeyParser :: ParsecT Text () Identity [Text]
packageKeyParser =
  let p = choice
        [ try $ (:) <$> (fromString <$> manyTill anyChar (try (string "/node_modules/"))) <*> p
        , pure <$> try (fromString <$> many1 anyChar <* eof)
        ]
   in string "node_modules/" *> p

data Package =
  Package
    { version   :: Text
    , resolved  :: Text
    , integrity :: Text
    } deriving stock    (Show, Generic)
      deriving anyclass (FromJSON)
