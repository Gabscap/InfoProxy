{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}

module Server where

import qualified Data.ByteString as BS
import qualified Data.Text as T
import qualified System.IO.Streams as Streams
import           System.IO.Streams (InputStream, OutputStream)

import Prelude hiding (getContents)
import Network.Simple.TCP as TCP hiding (send)

import Control.Exception.Lifted
import Control.Monad.Reader

import Data.Int

import qualified Control.Concurrent.Async.Lifted as L
import System.Timeout.Lifted

import qualified Config as C
import MCPacket

type InstanceT = ReaderT ServerConfig
type InOut     = (InputStream BS.ByteString, OutputStream BS.ByteString)

data ServerConfig = ServerConfig { instanceId  :: Int
                                 , address     :: C.Address
                                 , kickMessage :: T.Text
                                 , status      :: T.Text }
    deriving (Show)

----
tryAny action         = L.withAsync action L.waitCatch
catchAny action onE   = tryAny action >>= either onE return
-----

startServer :: InstanceT IO ()
startServer = do
    sc <- ask
    a@C.Address{..} <- asks address

    msg $ "Listening on " ++ show a
    TCP.listen (Host host) (show port) $ \(sock, _) ->
        (acceptLoop sock $ flip runReaderT sc . handleClient)
        `catch` (\(_ :: ShutdownException) -> return ())

    msg $ "Server closed on " ++ show a

acceptLoop :: MonadIO m => Socket -> ((Socket, SockAddr) -> IO ()) -> m a
acceptLoop sock handle = loop
    where loop = do
            acceptFork sock handle
            loop

handleClient :: (Socket, SockAddr) -> InstanceT IO ()
handleClient (sock, client) = do
    msg $ show client ++ " connected"
    (input, output) <- liftIO $ Streams.socketToStreams sock

    timeout 5000000 (handleHandshake (input, output) `catchAny` eHandler) >>= \case
        Nothing -> msg $ show client ++ " timed out"
        Just _  -> return ()
    where eHandler (_ :: SomeException) = return ()


handleHandshake :: InOut -> InstanceT IO ()
handleHandshake i@(input, _) = do
    SHandshake{..} <- liftIO $ readPacket input
    nextState hNext i

-- Status
handleStatus :: InOut -> InstanceT IO ()
handleStatus s@(input, _) = do
    request@SRequest <- liftIO $ readPacket input
    handleStatusPacket request s
    ping@SPing{}     <- liftIO $ readPacket input
    handleStatusPacket ping s

handleStatusPacket :: S1Packet -> InOut -> InstanceT IO ()
handleStatusPacket SRequest (_, output) = do
    packet <- CResponse <$> asks status
    liftIO $ writePacket output packet

handleStatusPacket (SPing i) (_, output) = liftIO $ writePacket output $ CPong i

-- Login
handleLogin :: InOut -> InstanceT IO ()
handleLogin (_, output) = do
    packet <- CDisconnect <$> asks kickMessage
    liftIO $ writePacket output packet

nextState :: Int32 -> InOut -> InstanceT IO ()
nextState 0 = handleHandshake
nextState 1 = handleStatus
nextState 2 = handleLogin
nextState i = throw.ReadException $ "Unsupported State: " ++ show i

msg :: MonadIO m => String -> InstanceT m ()
msg m = do
    id <- asks instanceId
    liftIO . putStrLn $ "[Server #" ++ show id ++ "] " ++ m

data ReadException = ReadException String
instance Show ReadException where
    show (ReadException msg) = "ReadException: " ++ msg
instance Exception ReadException

data ShutdownException = ShutdownException
instance Show ShutdownException where
    show _ = "ShutdownException"
instance Exception ShutdownException
