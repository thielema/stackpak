{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
module GhcData where

import GHC.Generics

import Data.Text (Text)
import qualified Data.Text as T

-- TODO: create and use a sum type for the architectures

-- |The available GHC architectures.
ghcArchitectures :: [Text]
ghcArchitectures = map T.pack ["i386", "x86_64", "armv7", "aarch64"]

-- |Convert the architecture to a Flatpak compatible one.
ghcArchitectureTranslated :: Text -> Text
ghcArchitectureTranslated "armv7" = "arm"
ghcArchitectureTranslated arch = arch

data GhcData = GhcData
    { architecture :: Text
    , version :: Text
    , url :: Text
    , hash :: Text
    } deriving (Generic, Show)
