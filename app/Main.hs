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
import Data.List (partition)
import Data.String.Conversions
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import System.Console.CmdArgs
import System.Directory

import DependencyResolver (resolve)
import Generator (generate)
import GeneratorInput
import GhcData
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
version = "1.0.3"

copyright :: Text
copyright = "(C) Richard Szibele"

defaultInput :: FilePath
defaultInput = "stackpak.json"

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
    } deriving (Data, Typeable, Show, Eq)
defaultArguments = Arguments
    { input = defaultInput &= typ "BASE_FLATPAK_FILE" &= argPos 0
    , directory = def &= typ "PROJECT_DIRECTORY" &= argPos 1
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
    rawLtsYaml   <- httpsGet $ urlLtsYaml (resolver stackYaml')
    ltsYaml'     <- decodeYaml $ cs rawLtsYaml
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
    void $ liftIO $ T.putStrLn "Resolving build order..."
    ExceptT $ pure $ Right pkgsInBuildOrder
    where
        pkgs = resolvePackages (ltsYaml projInfo) (stackLsDeps projInfo)
        pkgsInBuildOrder = resolve pkgs

-- |Resolve the hashes of the packages.
resolveHashes :: Text -> [Package] -> ExceptT Text IO (HMS.HashMap Text Text)
resolveHashes stackRoot pkgs = do
    liftIO $ T.putStrLn "Resolving package hashes..."
    hashes <- mapM (\pkg -> do
        hash <- tryHashFile $ filePath pkg
        pure $ case hash of
             Left msg -> (T.concat [pkgName pkg, "-", pkgVersion pkg], "") -- we want version info for printing if failed
             Right h  -> (pkgName pkg, h)
        ) pkgs
    let (resolved, unresolved) = Data.List.partition (\(n, h) -> h /= "") hashes
    -- warn user
    -- It's _not_ a critical error if the package is missing, it could be part of the
    -- base GHC packages that are already available and stack just uses that.
    -- If stack uses a different version then it will be available.
    liftIO $ if length unresolved > 0
        then do
            let unresolvedMsg = T.intercalate ", " $ map fst unresolved
            T.putStrLn $ T.concat ["Warning: The following packages are _not_ in the stack packages directory and will be ignored in the resulting build manifest: ", unresolvedMsg]
        else pure ()
    pure $ HMS.fromList resolved
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
    let outputFile = T.pack $ Main.output arguments

    -- get necessary project information.
    projInfo <- setup
        (T.pack $ Main.input arguments)
        (T.pack $ directory arguments)
        (pathStackYaml $ T.pack $ directory arguments)
        (pathStackProjectYaml $ T.pack $ directory arguments)

    -- get latest commit from the https://github.com/commercialhaskell/all-cabal-files repository.
    commit <- resolveCabalFilesGitCommit

    -- resolve the available GHCs.
    ghcVersion <- liftExceptT $ resolveGhcVersion $ ltsYaml projInfo
    allTryGhcs <- liftIO $ mapM (\arch -> do
            let badResult = do
                    T.putStrLn $ T.concat ["Warning: ignoring architecture \"", arch, "\""]
                    pure []
            ghcUrl <- runExceptT $ liftExceptT $ urlGhcArchive arch ghcVersion
            case ghcUrl of
                Left _    -> badResult
                Right url -> do
                    ghcHash <- runExceptT $ resolveGhcHash arch ghcVersion url
                    case ghcHash of
                        Left _     -> badResult
                        Right hash -> pure $ [GhcData
                            { architecture = arch
                            , version = ghcVersion
                            , url = url
                            , hash = hash
                            }]
        ) ghcArchitectures
    let allGhcs = concat allTryGhcs

    -- resolve the build order.
    pkgs <- resolveBuildOrder projInfo

    -- resolve the hashes of the packages.
    pkgHashes <- resolveHashes (stackRoot projInfo) pkgs
    let pkgsWithHashes = filter (\x -> (Stack.LtsYaml.name :: Package -> Text) x `elem` (HMS.keys pkgHashes)) pkgs

    -- resolve the hashes of the .cabal revisions.
    revisionHashes <- resolveCabalRevisionHashes commit pkgsWithHashes

    -- generate the manifest given the project information and the packages in the correct build order.
    manifest <- generateManifest $ GeneratorInput
        { projectInformation = projInfo
        , packageHashes = pkgHashes
        , revisionHashes = revisionHashes
        , revisionCommit = commit
        , packages = pkgsWithHashes
        , ghcs = allGhcs
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
