{-# LANGUAGE DeriveGeneric #-}
module Stack.StackYaml where

import Data.Text (Text)
import Data.Yaml
import GHC.Generics

data StackYaml = StackYaml 
    { resolver :: Text
    , arch :: Maybe Text
    } deriving (Generic, Show)
instance FromJSON StackYaml
