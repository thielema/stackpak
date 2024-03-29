{-# LANGUAGE OverloadedStrings #-}
module Util where

import Control.Exception
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Except
import Crypto.Hash
import Data.Aeson
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy.Char8 as L8
import Data.Foldable
import Data.List
import Data.String.Conversions
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Yaml as Yaml
import GHC.IO.Handle
import Network.HTTP.Client
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Network.HTTP.Types.Status (statusCode)
import System.Directory
import System.Environment
import System.Exit
import System.Process

-- |Helper function to convert an Either String a to Either Text a
textEither :: Either String a -> Either Text a
textEither (Left str) = Left $ T.pack str
textEither (Right x) = Right x

-- |Helper function to lift an Either into the ExceptT monad.
liftExceptT :: Either a b -> ExceptT a IO b
liftExceptT x = ExceptT $ pure $ x

-- |Load a file and return its contents.
loadFile :: Text -> ExceptT Text IO B.ByteString
loadFile filePath = do
    readResult <- liftIO $ (try (B.readFile $ T.unpack filePath) :: IO (Either SomeException B.ByteString))
    liftExceptT $ case readResult of
        Left _  -> Left $ T.pack $ "Failed to read file: " ++ T.unpack filePath
        Right x -> Right x

-- |Load a file and return its contents as text.
loadTextFile :: Text -> ExceptT Text IO Text
loadTextFile filePath = loadFile filePath >>= return . cs

-- |Decodes Yaml text.
decodeYaml :: FromJSON a => Text -> ExceptT Text IO a
decodeYaml rawYaml = liftExceptT $ textEither $ Yaml.decodeEither (cs rawYaml)

-- |Load a Yaml file and decode its contents.
loadYaml :: FromJSON a => Text -> ExceptT Text IO a
loadYaml filePath = do
    liftIO $ T.putStrLn $ T.concat ["Loading json: ", filePath, "..."]
    loadResult <- loadTextFile filePath
    decodeYaml (cs loadResult)

-- |Load a Json File and decode its contents.
loadJson :: FromJSON a => Text -> ExceptT Text IO a
loadJson filePath = do
    liftIO $ T.putStrLn $ T.concat ["Loading json: ", filePath, "..."]
    loadResult <- loadTextFile filePath
    liftExceptT $ textEither $ eitherDecode (cs loadResult)

-- |Load a hash file from disk.
loadHashFile :: Text -> ExceptT Text IO Text
loadHashFile filePath = do
    liftIO $ T.putStrLn $ T.concat ["Loading hash: ", filePath, "..."]
    loadTextFile filePath

-- |Save a file.
saveFile :: Text -> B.ByteString -> ExceptT Text IO ()
saveFile filePath fileContents = do
    writeResult <- liftIO $ (try (B.writeFile (T.unpack filePath) fileContents) :: IO (Either SomeException ()))
    liftExceptT $ case writeResult of
        Left _  -> Left $ T.concat ["Failed to write file: ", filePath]
        Right _ -> Right ()

-- |Load and hash a file.
hashFile :: Text -> ExceptT Text IO Text
hashFile filePath = loadFile filePath >>= pure . T.pack . show . hashWith SHA256

-- |Try and hash a file without failing.
tryHashFile :: Text -> ExceptT Text IO (Either Text Text)
tryHashFile filepath = liftIO $ runExceptT $ hashFile filepath

-- |Hash bytestring in memory.
hashData :: B.ByteString -> ExceptT Text IO Text
hashData contents = return $ T.pack $ show $ hashWith SHA256 contents

-- |Get the contents of a HTTPS request.
httpsGet :: Text -> ExceptT Text IO L8.ByteString
httpsGet url = do
    liftIO $ T.putStrLn $ T.concat ["Fetching: ", url, "..."]
    response <- liftIO $ do
        manager <- newManager tlsManagerSettings
        request <- parseRequest (T.unpack url)
        resp <- httpLbs request manager
        pure resp
    case statusCode $ responseStatus response of
        200  -> return $ responseBody response
        code -> throwE $ T.concat ["HTTPS get failed with status code: ", T.pack $ show code]

-- |Download the contents of a url and save it to a file.
downloadToFile :: Text -> Text -> ExceptT Text IO ()
downloadToFile url filePath = do
    contents <- httpsGet url
    saveFile filePath (L8.toStrict contents)

-- |Create an instance of a process.
mkProcess :: Text -> Text -> [Text] -> CreateProcess
mkProcess workingDir procName cmds = (proc (T.unpack procName) (map T.unpack cmds))
    { cwd = Just (T.unpack workingDir)
    , std_in = NoStream
    , std_out = CreatePipe
    , std_err = CreatePipe
    }

-- |Create an instance of a shell process.
mkShellProcess :: Maybe Text -> Text -> CreateProcess
mkShellProcess workingDir command =
    (shell $ T.unpack command)
    { cwd = T.unpack <$> workingDir
    , std_in = NoStream
    , std_out = CreatePipe
    , std_err = CreatePipe
    }

-- |Execute the process and return the output as text.
execProcess :: CreateProcess -> ExceptT Text IO Text
execProcess process = do
    -- check if we are running as a Flatpak
    insideFlatpak <- liftIO $ lookupEnv "FLATPAK_ID"
    let fpArgs = case (cmdspec process) of
                     ShellCommand str      -> [str]
                     RawCommand   exe args -> exe : args
    let p = case insideFlatpak of
                Nothing -> process
                Just _  -> process { cmdspec = ShellCommand $ "flatpak-spawn " ++ intercalate " " (["--host", "--"] ++ fpArgs) }
    -- create it
    (_, mHandleOut, mHandleErr, processHandle) <- liftIO $ createProcess p
    exitCode <- liftIO $ waitForProcess processHandle
    output <- liftIO $ traverse hGetContents mHandleOut
    error <- liftIO $ traverse hGetContents mHandleErr
    ExceptT $ pure $ case exitCode == ExitSuccess of
        False -> Left $ T.pack $
            ("Failed to execute `" <> (show $ cmdspec p) <> "` in: " <> (show $ cwd p))
            ++ fold error
        True  -> Right $ foldMap T.pack output
