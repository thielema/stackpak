module ProjectInformation where

import Data.Text (Text)

import Flatpak.Flatpak
import Stack.StackLsDependency
import Stack.StackYaml
import Stack.PackageYaml
import Stack.LtsYaml

-- |Data to be used for resolving and creating the manifest.
data ProjectInformation = ProjectInformation
    { stackRoot :: Text
    , stackLsDeps :: [StackLsDependency]
    , stackYaml :: StackYaml
    , packageYaml :: PackageYaml
    , ltsYaml :: LtsYaml
    , baseFlatpak :: Flatpak
    }
