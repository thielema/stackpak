{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Main where

import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Except
import GHC.Generics
import qualified Data.ByteString.Lazy as BL
import qualified Data.HashMap.Strict as HMS
import Data.String.Conversions
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import System.Console.CmdArgs
import System.Directory

import DependencyResolver (resolve)
import Generator (generate)
import GeneratorInput
import ProjectInformation
import Stack.Stack (stackBuild, stackLsDependencies)
import Stack.StackYaml (resolver)
import Stack.LtsYaml
import Paths
import Urls
import Util

--remove me
import qualified Data.HashMap.Strict as HMS

name :: Text
name = "stackpak"

version :: Text
version = "0.0.1"

copyright :: Text
copyright = "(C) Richard Szibele"

defaultInput :: FilePath
defaultInput = "stackpak.json"

defaultArchitecture :: String
defaultArchitecture = "x86_64"

defaultOutput :: FilePath
defaultOutput = "stackpak.json"

-- |Program arguments.
data Arguments = Arguments
    { input :: FilePath
    -- ^ The base Flatpak manifest.
    , directory :: FilePath
    -- ^ Directory of the stack project to generate the manifest for.
    , output :: FilePath
    -- ^ The output file.
    , architecture :: String
    -- ^ The architecture to generate for {i386, x86_64}.
    } deriving (Data, Typeable, Show, Eq)
defaultArguments = Arguments
    { input = defaultInput &= typ "BASE_FLATPAK_FILE" &= argPos 0
    , directory = def &= typ "PROJECT_DIRECTORY" &= argPos 1
    , architecture = defaultArchitecture &= opt defaultArchitecture &= typ defaultArchitecture &= help "Architecture to generate for {i386, x86_64}"
    , output = defaultOutput &= opt defaultOutput &= typ defaultOutput &= help "File to output to."
    } &= 
    program (T.unpack Main.name) &=
    help "Generate a Flatpak manifest from a stack project." &=
    summary (T.unpack $ T.concat ["Stackpak v", Main.version, ", ", copyright])

-- |Gather the necessary information of the project.
setup :: Text -> Text -> Text -> Text -> ExceptT Text IO ProjectInformation
setup baseFlatpakFilePath stackDirectory stackYaml packageYaml = do
    stackRoot'   <- liftIO $ pathStackRoot
    _            <- stackBuild stackDirectory
    stackLsDeps' <- stackLsDependencies stackDirectory
    stackYaml'   <- loadYaml stackYaml
    packageYaml' <- loadYaml packageYaml
    ltsYaml'     <- loadYaml $ pathStackLtsYaml stackRoot' (resolver stackYaml')
    baseFlatpak' <- loadJson baseFlatpakFilePath

    ExceptT $ pure $ Right $ ProjectInformation
        { stackRoot = stackRoot'
        , stackLsDeps = stackLsDeps'
        , stackYaml = stackYaml'
        , packageYaml = packageYaml'
        , ltsYaml = ltsYaml'
        , baseFlatpak = baseFlatpak'
        }

-- |Resolve build order with a given information about the project.
resolveBuildOrder :: ProjectInformation -> ExceptT Text IO [Package]
resolveBuildOrder projInfo = do
    _ <- liftIO $ T.putStrLn "Resolving build order..."
    ExceptT $ pure $ Right pkgsInBuildOrder
    where
        pkgs = resolvePackages (ltsYaml projInfo) (stackLsDeps projInfo)
        pkgsInBuildOrder = resolve pkgs

-- |Resolve the hashes of the packages.
resolveHashes :: Text -> [Package] -> ExceptT Text IO (HMS.HashMap Text Text)
resolveHashes stackRoot pkgs = do
    liftIO $ T.putStrLn "Resolving package hashes..."
    hashes <- mapM (\pkg -> do
        hash <- hashFile $ filePath pkg
        pure $ (pkgName pkg, hash)
        ) pkgs
    pure $ HMS.fromList hashes
    where
        pkgName pkg = (Stack.LtsYaml.name :: Package -> Text) pkg
        pkgVersion pkg = (Stack.LtsYaml.version :: Package -> Text) pkg
        filePath pkg = T.concat [ stackRoot
                                , "/indices/Hackage/packages/"
                                , (pkgName pkg), "/"
                                , (pkgVersion pkg), "/"
                                , (pkgName pkg), "-", (pkgVersion pkg), ".tar.gz"
                                ]

-- |Resolve the latest commit in the git repository: https://github.com/commercialhaskell/all-cabal-files/tree/hackage
-- |TODO: preferably use a git package on Hackage to resolve this.
resolveCabalFilesGitCommit :: ExceptT Text IO Text
resolveCabalFilesGitCommit = do
    liftIO $ T.putStrLn "Resolving latest cabal-files git commit..."
    commit <- execProcess $ mkShellProcess Nothing "git ls-remote https://github.com/commercialhaskell/all-cabal-files.git | grep refs/heads/hackage | cut -f 1 | tr -d '\n'"
    liftIO $ T.putStrLn $ T.concat ["The latest commit is: ", commit]
    pure commit

-- |Resolve the GHC Hash.
resolveGhcHash :: Text -> Text -> Text -> ExceptT Text IO Text
resolveGhcHash arch version url = do
    liftIO $ T.putStrLn "Resolving GHC archive hash..."
    -- get the path to our cache directory and create it.
    dir <- liftIO $ pathCache
    liftIO $ createDirectoryIfMissing True (T.unpack $ pathGhcHashDirectory dir)
    -- download the ghc archive and save it in our cache directory
    let filePath = pathGhcHashFile dir arch version
    fileExists <- liftIO $ doesFileExist (T.unpack filePath) 
    if fileExists
        then loadHashFile filePath
        else do
            content <- httpsGet url
            h <- hashData (BL.toStrict content)
            saveFile filePath (cs h)
            pure h

-- |Resolve the hashes of the .cabal files.
resolveCabalRevisionHashes :: Text -> [Package] -> ExceptT Text IO (HMS.HashMap Text Text)
resolveCabalRevisionHashes commit pkgs = do
        liftIO $ T.putStrLn "Resolving .cabal revision hashes..."
        -- get the path to our cabal-files cache directory and create it.
        dir <- liftIO $ pathCache
        liftIO $ createDirectoryIfMissing True (T.unpack $ pathCabalHashDirectory dir commit)
        -- go through all packages and retrieve the hashes.
        -- use the cache when available.
        resultTuples <- mapM (\p -> do
                let filePath = cabalHashFilePath dir p
                fileExists <- liftIO $ doesFileExist (T.unpack filePath)
                hash <- if fileExists
                    then loadHashFile filePath
                    else do
                        content <- httpsGet $ pkgUrl p
                        h <- hashData (BL.toStrict content)
                        saveFile filePath (cs h)
                        pure h
                ExceptT $ pure $ Right (pkgName p, hash)
            ) pkgs
        ExceptT $ pure $ Right $ HMS.fromList resultTuples
    where
        pkgName pkg = (Stack.LtsYaml.name :: Package -> Text) pkg
        pkgVersion pkg = (Stack.LtsYaml.version :: Package -> Text) pkg
        pkgUrl pkg = urlCabalMetadata commit (pkgName pkg) (pkgVersion pkg)
        cabalHashFilePath cacheDir pkg = pathCabalHashFile cacheDir commit (pkgName pkg) (pkgVersion pkg)

-- |Generate the manifest.
generateManifest :: GeneratorInput -> ExceptT Text IO Text
generateManifest generatorInput = do
    liftIO $ T.putStrLn "Generating manifest..."
    liftExceptT $ generate generatorInput

-- |Save the generated manifest.
saveManifest :: Text -> Text -> ExceptT Text IO ()
saveManifest filePath manifest = do
    liftIO $ T.putStrLn $ T.concat ["Writing manifest to: ", filePath, "..."]
    saveFile filePath (cs manifest)

-- |Execute everything.
execute :: Arguments -> ExceptT Text IO ()
execute arguments = do
    -- get some args.
    let arch = T.pack $ Main.architecture arguments
    let outputFile = T.pack $ Main.output arguments

    -- get necessary project information.
    projInfo <- setup
        (T.pack $ Main.input arguments)
        (T.pack $ directory arguments)
        (pathStackYaml $ T.pack $ directory arguments)
        (pathStackProjectYaml $ T.pack $ directory arguments)

    -- get latest commit from the https://github.com/commercialhaskell/all-cabal-files repository.
    commit <- resolveCabalFilesGitCommit

    -- resolve the GHC version and get its hash.
    ghcVersion <- liftExceptT $ resolveGhcVersion $ ltsYaml projInfo
    ghcUrl <- liftExceptT $ urlGhcArchive arch ghcVersion
    ghcHash <- resolveGhcHash arch ghcVersion ghcUrl

    -- resolve the build order.
    pkgs <- resolveBuildOrder projInfo

    -- resolve the hashes of the packages.
    pkgHashes <- resolveHashes (stackRoot projInfo) pkgs

    -- resolve the hashes of the .cabal revisions.
    revisionHashes <- resolveCabalRevisionHashes commit pkgs

    -- generate the manifest given the project information and the packages in the correct build order.
    manifest <- generateManifest $ GeneratorInput
        { projectInformation = projInfo
        , packageHashes = pkgHashes
        , revisionHashes = revisionHashes
        , revisionCommit = commit
        , packages = pkgs
        , architecture = arch
        , ghcVersion = ghcVersion
        , ghcHash = ghcHash
        , ghcUrl = ghcUrl
        }

    -- save generated manifest to a file.
    saveManifest outputFile manifest

    -- 
    liftIO $ T.putStrLn "Done."

    ExceptT $ pure $ Right () -- TODO: remove me

-- |Main entry point.
main = do
    args <- cmdArgs defaultArguments
    result <- runExceptT $ execute args
    case result of
        Left msg -> T.putStrLn msg
        Right _ -> pure ()
