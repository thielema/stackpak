{-# LANGUAGE DeriveGeneric #-}
module Stack.PackageYaml where

import Data.Text (Text)
import Data.Yaml
import GHC.Generics

data PackageYaml = PackageYaml 
    { name :: Text
    , version :: Text
    , license :: Text
    , author :: Text
    , maintainer :: Text
    , copyright :: Text
    , dependencies :: [Text]
    } deriving (Generic, Show)
instance FromJSON PackageYaml
