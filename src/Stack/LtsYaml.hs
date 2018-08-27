{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DuplicateRecordFields #-}
module Stack.LtsYaml where

import qualified Data.HashMap.Strict as HMS
import Data.List
import Data.Text (Text)
import Data.Yaml
import GHC.Generics

import Stack.StackLsDependency (StackLsDependency)
import qualified Stack.StackLsDependency as StackLsDependency

data CabalFileInfoHashes = CabalFileInfoHashes
    { md5 :: Text
    , skein512_512 :: Text
    , sha1 :: Text
    , sha512 :: Text
    , sha256 :: Text
    , gitSha1 :: Text
    } deriving (Generic, Show)
instance FromJSON CabalFileInfoHashes where
    parseJSON = withObject "" $ \o -> CabalFileInfoHashes
        <$> o .: "MD5"
        <*> o .: "Skein512_512"
        <*> o .: "SHA1"
        <*> o .: "SHA512"
        <*> o .: "SHA256"
        <*> o .: "GitSHA1"

data CabalFileInfo = CabalFileInfo
    { size :: Int
    , hashes :: CabalFileInfoHashes 
    } deriving (Generic, Show)
instance FromJSON CabalFileInfo

data DependencyPackage = DependencyPackage 
    { name :: Text
    } deriving (Generic, Show)

parseDependencyPackage :: (Text, Value) -> Parser DependencyPackage
parseDependencyPackage (k, v) = withObject "entry body" (\o -> return $ DependencyPackage k) v

newtype DependencyPackages = DependencyPackages [DependencyPackage] deriving (Generic, Show)
instance FromJSON DependencyPackages where
    parseJSON x = parseJSON x >>= mapM parseDependencyPackage . HMS.toList >>= (\y -> pure $ DependencyPackages y)

getDependencyPackages :: DependencyPackages -> [DependencyPackage]
getDependencyPackages (DependencyPackages p) = p

data Description = Description
    { cabalVersion :: Maybe Text
    , providedExes :: Maybe Text
    , packages :: DependencyPackages
    } deriving (Generic, Show)
instance FromJSON Description

data Package = Package
    { name :: Text
    , version :: Text
    --, cabalFileInfo :: CabalFileInfo
    , simpleBuild :: Bool
    , description :: Description
    } deriving (Generic, Show)

parsePackage :: (Text, Value) -> Parser Package
parsePackage (k, v) = withObject "entry body" (\o -> Package k
    <$> o .: "version"
    -- <*> o .: "cabal-file-info"
    <*> o .: "simple-build"
    <*> o .: "description") v

newtype Packages = Packages [Package] deriving (Generic, Show)
instance FromJSON Packages where
    parseJSON x = parseJSON x >>= mapM parsePackage . HMS.toList >>= (\y -> pure $ Packages y)

getPackages :: Packages -> [Package]
getPackages (Packages p) = p

data CorePackage = CorePackage
    { name :: Text
    , version :: Text
    } deriving (Generic, Show)

parseCorePackage :: (Text, Value) -> Parser CorePackage
parseCorePackage (k, String v) = pure $ CorePackage k v -- TODO: FIXME: inexhaustive pattern matching

newtype CorePackages = CorePackages [CorePackage] deriving (Generic, Show)
instance FromJSON CorePackages where
    parseJSON o = parseJSON o >>= mapM parseCorePackage . HMS.toList >>= (\x -> pure $ CorePackages x)

data SystemInfo = SystemInfo
    { corePackages :: CorePackages
    } deriving (Generic, Show)
instance FromJSON SystemInfo where
    parseJSON = withObject "" $ \o -> SystemInfo
        <$> o .: "core-packages"

data LtsYaml = LtsYaml
    { systeminfo :: SystemInfo
    , packages :: Packages
    } deriving (Generic, Show)
instance FromJSON LtsYaml where
    parseJSON = withObject "" $ \o -> LtsYaml
        <$> o .: "system-info"
        <*> o .: "packages"

-- |Given a package, return its dependencies.
getPackageDependencies :: Package -> [DependencyPackage]
getPackageDependencies pkg = result
    where
        DependencyPackages result = (packages :: Description -> DependencyPackages) $ description pkg

-- |Maybe return the GHC version given an lts.
resolveGhcVersion :: LtsYaml -> Either Text Text
resolveGhcVersion lts = ghcPackage
    where 
        sysInfo = systeminfo lts
        (CorePackages cPkgs) = corePackages sysInfo
        ghcPackage = case filter (\cpkg -> (name :: CorePackage -> Text) cpkg == "ghc") cPkgs of
            [x] -> Right $ (version :: CorePackage -> Text) x
            _   -> Left "Failed to find the GHC version in the core packages of the LTS Yaml."

-- |Given the LTS and a list of StackLsDependency, return the corresponding packages.
resolvePackages :: LtsYaml ->  [StackLsDependency] -> [Package]
resolvePackages lts stackDeps = filter (\pkg -> (pkgName pkg) `elem` pkgNames) (getPackages $ pkgs lts) 
    where
        pkgName :: Package -> Text
        pkgName = name
        pkgs :: LtsYaml -> Packages
        pkgs = packages
        pkgNames = map StackLsDependency.name stackDeps
