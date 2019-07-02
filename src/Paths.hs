{-# LANGUAGE OverloadedStrings #-}
module Paths where

import Data.Text (Text)
import qualified Data.Text as T
import System.Directory

-- TODO: use strongly typed file paths

-- |Determines the configuration directory for Stackpak.
pathConfig :: IO Text
pathConfig = getXdgDirectory XdgConfig "stackpak" >>= pure . T.pack

-- |Determines the cache directory for Stackpak.
pathCache :: IO Text
pathCache = getXdgDirectory XdgCache "stackpak" >>= pure . T.pack

-- |Determine the stack root directory.
pathStackRoot :: IO Text
pathStackRoot = getHomeDirectory >>= (\dir -> pure $ T.concat [T.pack dir, "/.stack"])

-- |Given the project root directory, return the path to the stack.yaml file.
pathStackYaml :: Text -> Text
pathStackYaml projectRoot = T.concat [projectRoot, "/stack.yaml"]

-- |Given the project root directory, return the path to the project.yaml file.
pathStackProjectYaml :: Text -> Text
pathStackProjectYaml projectRoot = T.concat [projectRoot, "/package.yaml"]

-- |Given the stack root and the lts, return the path to the lts yaml.
pathStackLtsYaml :: Text -> Text -> Text
pathStackLtsYaml stackRoot lts = T.concat [stackRoot, "/build-plan/", lts, ".yaml"]

-- |Given the cache directory, return the path to the ghc hashes folder.
pathGhcHashDirectory :: Text -> Text
pathGhcHashDirectory cacheDir = T.concat [cacheDir, "/ghc-hashes"] 

-- |Given the cache directory, architecture and the version, return the path to the ghc archive.
pathGhcHashFile :: Text -> Text -> Text -> Text
pathGhcHashFile cacheDir arch version = T.concat [pathGhcHashDirectory cacheDir, "/ghc-", version, "-", arch, ".tar.xz.sha256"]

-- |Given the cache directory and the git commit, return the path to the hash folder of the cabal files.
pathCabalHashDirectory :: Text -> Text -> Text
pathCabalHashDirectory cacheDir commit = T.concat [cacheDir, "/cabal-hashes/", commit]

-- |Given the cache directory, the git commit for the cabal files, the package name and version, return the path to the hash.
pathCabalHashFile :: Text -> Text -> Text -> Text -> Text
pathCabalHashFile cacheDir commit pkgName pkgVersion =  T.concat [pathCabalHashDirectory cacheDir commit, "/", pkgName, "-", pkgVersion, ".cabal.sha256"]
