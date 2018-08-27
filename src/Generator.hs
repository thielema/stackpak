{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE DuplicateRecordFields #-}
module Generator where

import Data.Aeson
import Data.Aeson.Encode.Pretty
import qualified Data.HashMap.Strict as HMS
import Data.String.Conversions
import Data.Text (Text)
import qualified Data.Text as T

import Flatpak.Flatpak
import GeneratorInput
import ProjectInformation
import Stack.LtsYaml
import Urls ( urlCabalMetadata
            , urlCabalPackage
            )

-- |Generate the module to download, compile and install GHC.
generateGhcModule :: Text -> Text -> Text -> Text -> FlatpakModule
generateGhcModule ghcUrl ghcArchitecture ghcVersion ghcHash = FlatpakModule
    { _name = "ghc-" <> ghcVersion
    , _onlyArches = []
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
        , _url = ghcUrl
        , _sha256 = ghcHash
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
        ghcCleanupCommands = map ("rm -rf " <>) $ concat 
            [ map ("/app/bin/" <>) -- bin
                [ "ghc"
                , T.concat ["ghc-", ghcVersion generatorInput] 
                , "ghc-pkg"
                , T.concat ["ghc-pkg-", ghcVersion generatorInput]
                , "ghci"
                , T.concat ["ghci-", ghcVersion generatorInput]
                , "haddock"
                , T.concat ["haddock-ghc-", ghcVersion generatorInput]
                , "hp2ps"
                , "hpc"
                , "hsc2hs"
                , "runghc"
                , T.concat ["runghc-", ghcVersion generatorInput]
                , "runhaskell"
                ]
            , map ("/app/lib/" <>) -- lib
                [ "debug"
                , T.concat ["ghc-", ghcVersion generatorInput]
                , T.concat [architecture generatorInput, "-linux-ghc-", ghcVersion generatorInput]
                ]
            , map ("/app/share/" <>) -- share
                [ T.concat ["doc/ghc-", ghcVersion generatorInput]
                , T.concat ["doc/", architecture generatorInput, "-linux-ghc-", ghcVersion generatorInput]
                , "man"
                , T.concat [architecture generatorInput, "-linux-ghc-", ghcVersion generatorInput]
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
                [ ghcCleanupCommands
                , (_cleanupCommands :: Flatpak -> [Text]) flatpakBase
                ]
            , _modules = concat
                [ [(generateGhcModule (ghcUrl generatorInput) (architecture generatorInput) (ghcVersion generatorInput) (ghcHash generatorInput))]
                , (map (generateModule (packageHashes generatorInput) (revisionHashes generatorInput) (revisionCommit generatorInput)) ((packages :: GeneratorInput -> [Package]) generatorInput))
                , _modules flatpakBase
                ]
            }
