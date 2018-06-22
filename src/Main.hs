{-# LANGUAGE ScopedTypeVariables, ViewPatterns #-}

module Main where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Base64 as B64
import qualified Data.Text as T
import qualified Data.Text.Encoding as T

import Control.Concurrent
import Control.Concurrent.Async
import Control.Exception
import Control.Monad.Extra (unlessM)
import Control.Monad.Reader

import Data.Aeson as JSON

import System.Directory (doesFileExist)
import System.Exit
import System.IO
import System.IO.Error
import System.Posix.Signals

import Config
import Logger
import MCText
import OptParse
import Server
import ServerPing

main :: IO ()
main = do
    Options{..} <- parseCLI
    config      <- readConfig oConfig

    (log, logCleanup) <- setupLogger
    serverConfigs <- zipWithM (serverToServerConfig log) [1..] $ servers config

    -- Shutdown Handler (SIGINT (Ctrl-C), SIGTERM)
    shutdown <- newEmptyMVar
    installHandler sigINT  (CatchOnce $ putMVar shutdown ()) Nothing
    installHandler sigTERM (CatchOnce $ putMVar shutdown ()) Nothing

    log ("Starting Servers..." :: String)
    threads <- forM serverConfigs $ async . runReaderT startServer
    let waitForWorkers = forM_ threads wait

    race (takeMVar shutdown) waitForWorkers >>= \case
        Left _ -> do
            putStrLn "Received Shutdown signal"
            forM_ threads $ flip cancelWith ShutdownException
        Right _ -> return ()

    log ("Bye!" :: String)
    logCleanup

readConfig :: FilePath -> IO Config
readConfig configPath = do
    unlessM (doesFileExist configPath) $ do
        putStrLn "No config.yml found. Created default config"
        BS.writeFile configPath sampleConfigText
            `catch` (handleWriteFileEx configPath >=> const exitFailure)

    fileContents <- BS.readFile configPath
        `catch` (handleReadFileEx configPath >=> const exitFailure)

    case parseConfigFile fileContents of
        Left err -> do
            putStrLn $ "error parsing " ++ configPath ++ ":\n" ++ err
            exitFailure
        Right c  -> return c


serverToServerConfig :: Logger String -> Int -> Server -> IO ServerConfig
serverToServerConfig logger instanceId Server{..} = do
    kickMsgBC <- jsonToText <$> toBC kickMessage

    iconContents <- (Just <$> BS.readFile serverIcon)
        `catch` (handleReadFileEx serverIcon >=> const (return Nothing))
    let base64Icon = T.append "data:image/png;base64,"
                     . T.decodeUtf8
                     . B64.encode <$> iconContents

    motdBC <- toBC motd
    let ping   = ServerPing (DMCText motdBC)
                            (Players maxplayers online Nothing)
                            (Version serverbrand protocol)
                            base64Icon
    let status = jsonToText ping

    let logger' = logger . (("[Server #" ++ show instanceId ++ "] ") ++)
    return $ ServerConfig instanceId address kickMsgBC status logger'
  where toBC t@(T.head -> '{') = either (error  . ("Invalid JSON: " ++)) return
                                   . JSON.eitherDecodeStrict
                                   . T.encodeUtf8 $ t
        toBC t                 = return . singleBaseComponent . fromLegacyText $ t


handleReadFileEx :: FilePath -> IOException -> IO ()
handleReadFileEx path (isDoesNotExistError -> True) =
    putErrLn $ path ++ " not found."
handleReadFileEx path (isPermissionError   -> True) =
    putErrLn $ "Permission error while reading " ++ path
handleReadFileEx path e =
    putErrLn $ "Error reading " ++ path ++ ":\n" ++ displayException e

handleWriteFileEx :: FilePath -> IOException -> IO ()
handleWriteFileEx path (isPermissionError   -> True) =
    putErrLn $ "Permission error while writing " ++ path
handleWriteFileEx path e =
    putErrLn $ "Error writing " ++ path ++ ":\n" ++ displayException e

jsonToText :: ToJSON a => a -> T.Text
jsonToText = T.decodeUtf8 . BSL.toStrict . JSON.encode

putErrLn :: String -> IO ()
putErrLn = hPutStrLn stderr
