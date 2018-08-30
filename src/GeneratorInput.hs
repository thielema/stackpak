module GeneratorInput where

import qualified Data.HashMap.Strict as HMS
import Data.Text (Text)

import GhcData
import ProjectInformation
import Stack.LtsYaml

-- |All input data for generating the manifest.
data GeneratorInput = GeneratorInput
    { projectInformation :: ProjectInformation
    , packageHashes :: HMS.HashMap Text Text
    , revisionHashes :: HMS.HashMap Text Text
    , revisionCommit :: Text
    , packages :: [Package]
    , ghcs :: [GhcData]
    }
