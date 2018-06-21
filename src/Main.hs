{-# LANGUAGE ScopedTypeVariables #-}

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
import MCText
import OptParse
import Server
import ServerPing


main :: IO ()
main = do
    Options{..} <- parseCLI
    config      <- readConfig oConfig
    serverConfigs <- mapM (uncurry serverToServerConfig) . zip [1..] $ servers config

    -- Shutdown Handler (SIGINT (Ctrl-C), SIGTERM)
    shutdown <- newEmptyMVar
    installHandler sigINT  (CatchOnce $ putMVar shutdown ()) Nothing
    installHandler sigTERM (CatchOnce $ putMVar shutdown ()) Nothing

    putStrLn "Starting Servers..."
    threads <- forM serverConfigs $ async . runReaderT startServer
    let waitForWorkers = forM_ threads wait

    race (takeMVar shutdown) waitForWorkers >>= \case
        Left _ -> do
            putStrLn "Received Shutdown signal"
            forM_ threads $ flip cancelWith ShutdownException
        Right _ -> return ()

    putStrLn "Bye!"

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


serverToServerConfig :: Int -> Server -> IO ServerConfig
serverToServerConfig instanceId Server{..} = do
    let toBC    = singleBaseComponent . fromLegacyText
    let kickMsg = jsonToText . toBC $ kickMessage

    iconContents <- (Just <$> BS.readFile serverIcon)
        `catch` (handleReadFileEx serverIcon >=> const (return Nothing))
    let base64Icon = (T.append "data:image/png;base64,")
                     . T.decodeUtf8
                     . B64.encode <$> iconContents

    let ping   = ServerPing (DMCText $ toBC motd)
                            (Players maxplayers online Nothing)
                            (Version serverbrand protocol)
                            base64Icon
    let status = jsonToText ping
    return $ ServerConfig instanceId address kickMsg status


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
