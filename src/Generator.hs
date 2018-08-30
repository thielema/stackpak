{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE DuplicateRecordFields #-}
module Generator where

import Data.Aeson
import Data.Aeson.Encode.Pretty
import qualified Data.HashMap.Strict as HMS
import Data.List
import Data.String.Conversions
import Data.Text (Text)
import qualified Data.Text as T

import Flatpak.Flatpak
import GeneratorInput
import GhcData
import ProjectInformation
import Stack.LtsYaml
import Urls ( urlCabalMetadata
            , urlCabalPackage
            )

ghcVersion ghc = (version :: GhcData -> Text) ghc
ghcArch ghc = (architecture :: GhcData -> Text) ghc
ghcUrl ghc = (url :: GhcData -> Text) ghc
ghcHash ghc = (hash :: GhcData -> Text) ghc

-- |Generate the module to download, compile and install GHC.
generateGhcModule :: GhcData -> FlatpakModule
generateGhcModule ghc = FlatpakModule
    { _name = T.concat ["ghc-",  ghcVersion ghc, "-", ghcArch ghc]
    , _onlyArches = [ghcArchitectureTranslated (ghcArch ghc)]
    , _buildsystem = "simple"
    , _builddir = False
    , _buildCommands = 
        [ "./configure --prefix=/app"
        , "make install"
        ]
    , _cleanup = []
    --, _cleanupCommands = []
    , _sources = [FlatpakSource
        { _type = "archive"
        , _url = ghcUrl ghc
        , _sha256 = ghcHash ghc
        }]
    }

-- |Generate a module to download, build and install a package from Hackage.
-- |Due to the cabal package "revision" not being part of a package, we
-- |have to download the package, extract and then overwrite the .cabal file with the latest revision.
generateModule :: HMS.HashMap Text Text -> HMS.HashMap Text Text -> Text -> Package -> FlatpakModule
generateModule pkgHashes revisionHashes commit pkg = FlatpakModule
    { _name = pkgName
    , _onlyArches = []
    , _buildsystem = "simple"
    , _builddir = True
    , _buildCommands =
        [ "ghc -threaded --make Setup"
        , "./Setup configure --prefix=/app"
        , "./Setup build"
        , "./Setup install"
        ]
    , _cleanup = []
    --, _cleanupCommands = []
    , _sources = [
        FlatpakSource
        { _type = "archive"
        , _url =  urlCabalPackage pN pV
        , _sha256 =  pkgHash
        },
        FlatpakSource
        { _type = "file"
        , _url =  urlCabalMetadata commit pN pV
        , _sha256 =  revisionHash
        }]
    }
    where
        pN = ((name :: Package -> Text) pkg)
        pV = (version :: Package -> Text) pkg
        pkgName = T.concat [pN, "-", pV]
        pkgHash = case HMS.lookup pN pkgHashes of -- TODO: Fixme
            Just x -> x
            Nothing -> ""
        revisionHash = case HMS.lookup pN revisionHashes of -- TODO: FIXME
            Just x -> x
            Nothing -> ""

-- |Generate the output manifest.
generate :: GeneratorInput -> Either Text Text
generate generatorInput = Right $ cs $ encodePretty' (defConfig { confCompare = compare }) (flatpak :: Flatpak) -- TODO: FIXME
    where
        ghcCleanupCommands :: GhcData -> [Text]
        ghcCleanupCommands ghc = map ("rm -rf " <>) $ concat 
            [ map ("/app/bin/" <>) -- bin
                [ "ghc"
                , T.concat ["ghc-", ghcVersion ghc] 
                , "ghc-pkg"
                , T.concat ["ghc-pkg-", ghcVersion ghc]
                , "ghci"
                , T.concat ["ghci-", ghcVersion ghc]
                , "haddock"
                , T.concat ["haddock-ghc-", ghcVersion ghc]
                , "hp2ps"
                , "hpc"
                , "hsc2hs"
                , "runghc"
                , T.concat ["runghc-", ghcVersion ghc]
                , "runhaskell"
                ]
            , map ("/app/lib/" <>) -- lib
                [ "debug"
                , T.concat ["ghc-", ghcVersion ghc]
                , T.concat [ghcArch ghc, "-linux-ghc-", ghcVersion ghc]
                ]
            , map ("/app/share/" <>) -- share
                [ T.concat ["doc/ghc-", ghcVersion ghc]
                , T.concat ["doc/", ghcArch ghc, "-linux-ghc-", ghcVersion ghc]
                , "man"
                , T.concat [ghcArch ghc, "-linux-ghc-", ghcVersion ghc]
                ]
            ]
        flatpakBase = baseFlatpak $ projectInformation generatorInput
        flatpak =
            Flatpak
            { _appId = _appId flatpakBase
            , _runtime = _runtime flatpakBase
            , _runtimeVersion = _runtimeVersion flatpakBase
            , _sdk = _sdk flatpakBase
            , _command = _command flatpakBase
            , _finishArgs = _finishArgs flatpakBase
            , _cleanup = (_cleanup :: Flatpak -> [Text]) flatpakBase
            , _cleanupCommands = concat
                [ nub $ concat $ (map ghcCleanupCommands) (ghcs generatorInput)
                , (_cleanupCommands :: Flatpak -> [Text]) flatpakBase
                ]
            , _modules = concat
                [ (map generateGhcModule (ghcs generatorInput))
                , (map (generateModule (packageHashes generatorInput) (revisionHashes generatorInput) (revisionCommit generatorInput)) ((packages :: GeneratorInput -> [Package]) generatorInput))
                , _modules flatpakBase
                ]
            }
