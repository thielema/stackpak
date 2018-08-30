{-# LANGUAGE OverloadedStrings #-}
module Urls where

import Data.Text (Text)
import qualified Data.Text as T

import GhcData

-- |Generate a URL for the cabal metadata.
urlCabalMetadata :: Text -> Text -> Text -> Text
urlCabalMetadata commit pkgName pkgVersion = T.concat
    [ "https://raw.githubusercontent.com/commercialhaskell/all-cabal-files/"
    , commit
    , "/"
    , pkgName
    , "/"
    , pkgVersion
    , "/"
    , pkgName
    , ".cabal"
    ]

-- |Generate a URL to a package.
urlCabalPackage :: Text -> Text -> Text
urlCabalPackage name version = T.concat
    [ "https://s3.amazonaws.com/hackage.fpcomplete.com/package/"
    , name, "-", version, ".tar.gz"]

-- |Generate a URL to a GHC source archive.
urlMkGhcArchive :: Text -> Text -> Text -> Text
urlMkGhcArchive os arch version = T.concat 
    [ "https://github.com/commercialhaskell/ghc/releases/download/ghc-"
    , version
    , "-release/ghc-"
    , version
    , "-"
    , arch
    , "-"
    , os
    , "-linux.tar.xz"
    ]

-- |Generate a URL to a GHC source archive.
urlGhcArchive :: Text -> Text -> Either Text Text
urlGhcArchive arch vers = do
    continue <- if arch `elem` ghcArchitectures
        then Right ()
        else Left $ T.concat ["Unsupported architecture: \"", arch, "\""] 
    case vers of
        "8.4.3"  -> Right $ mk "deb8" arch vers
        "8.4.2"  -> Right $ mk "deb8" arch vers
        "8.4.1"  -> Right $ mk "deb8" arch vers
        "8.2.2"  -> Right $ mk "deb8" arch vers
        "8.2.1"  -> Right $ mk "deb8" arch vers
        "8.0.2"  -> Right $ mk "deb8" arch vers
        "8.0.1"  -> Right $ mk "deb7" arch vers
        "7.10.3" -> Right $ mk "deb7" arch vers
        "7.10.2" -> Right $ mk "deb7" arch vers
        "7.10.1" -> Right $ mk "deb7" arch vers
        "7.8.4"  -> Right $ mk "deb7" arch vers
        _ -> Left $ T.concat ["Unsupported GHC version: \"", vers, "\""]
    where
        mk = urlMkGhcArchive
