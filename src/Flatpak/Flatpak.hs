{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DuplicateRecordFields #-}
module Flatpak.Flatpak where

import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics
import Data.Aeson

data FlatpakSource = FlatpakSource
    { _type :: Text
    , _url :: Text
    , _sha256 :: Text
    } deriving (Generic, Show)
instance FromJSON FlatpakSource where
    parseJSON = withObject "" $ \o -> FlatpakSource
        <$> o .: "type"
        <*> o .: "url"
        <*> o .: "sha256"
instance ToJSON FlatpakSource where
    toJSON fps = object
        [ "type" .= _type fps
        , "url" .= _url fps
        , "sha256" .= _sha256 fps
        ]

data FlatpakModule = FlatpakModule
    { _name :: Text
    , _onlyArches :: [Text]
    , _buildsystem :: Text
    , _builddir :: Bool
    , _buildCommands :: [Text]
    , _cleanup :: [Text]
    --, _cleanupCommands :: [Text]
    , _sources :: [FlatpakSource]
    } deriving (Generic, Show)
instance FromJSON FlatpakModule where
    parseJSON = withObject "" $ \o -> FlatpakModule
        <$> o .: "name"
        <*> o .: "only-arches"
        <*> o .: "buildsystem"
        <*> o .: "builddir"
        <*> o .: "build-commands"
        <*> o .: "cleanup"
        -- <*> o .: "cleanup-commands"
        <*> o .: "sources"
instance ToJSON FlatpakModule where
    toJSON fpm = object
        [ "name" .= _name fpm
        , "only-arches" .= _onlyArches fpm
        , "buildsystem" .= _buildsystem fpm
        , "build-commands" .= _buildCommands fpm
        , "cleanup" .= (_cleanup :: FlatpakModule -> [Text] ) fpm
        --, "cleanup-commands" .= (_cleanupCommands :: FlatpakModule -> [Text]) fpm
        , "sources" .= _sources fpm
        ]

data Flatpak = Flatpak
    { _appId :: Text
    , _runtime :: Text
    , _runtimeVersion :: Text
    , _sdk :: Text
    , _command :: Text
    , _finishArgs :: [Text]
    , _cleanup :: [Text]
    , _cleanupCommands :: [Text]
    , _modules :: [FlatpakModule]
    } deriving (Generic, Show)
instance FromJSON Flatpak where
    parseJSON = withObject "" $ \o -> Flatpak
        <$> o .: "app-id"
        <*> o .: "runtime"
        <*> o .: "runtime-version"
        <*> o .: "sdk"
        <*> o .: "command"
        <*> o .: "finish-args"
        <*> o .: "cleanup"
        <*> o .: "cleanup-commands"
        <*> o .: "modules"
instance ToJSON Flatpak where
    toJSON fp = object
        [ "app-id" .= _appId fp
        , "runtime" .= _runtime fp
        , "runtime-version" .= _runtimeVersion fp
        , "sdk" .= _sdk fp
        , "command" .= _command fp
        , "finish-args" .= _finishArgs fp
        , "cleanup" .= (_cleanup :: Flatpak -> [Text]) fp
        , "cleanup-commands" .= (_cleanupCommands :: Flatpak -> [Text]) fp
        , "modules" .= _modules fp
        ]
