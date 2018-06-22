{-# LANGUAGE RankNTypes #-}

module Logger(Logger, setupLogger) where

import Data.Semigroup
import Control.Monad
import System.Log.FastLogger

type Logger a = a -> IO ()

setupLogger :: ToLogStr a => IO (Logger a, IO ())
setupLogger = do
    timeFormatter <- newTimeCache "%F %T"
    (logger', cleanup) <- newTimedFastLogger timeFormatter (LogStdout defaultBufSize)

    (loggerFile', cleanupFile) <- newTimedFastLogger timeFormatter (LogFile defaultLogSpec defaultBufSize)

    let timeify logger msg = logger $ \time -> logFormat time msg

    let logger     = timeify logger'
    let loggerFile = timeify loggerFile'

    return (liftM2 (>>) logger loggerFile, cleanup >> cleanupFile)

logFormat :: ToLogStr a => FormattedTime -> a -> LogStr
logFormat time msg = l <> toLogStr time <> r <> toLogStr msg <> endl
  where l = toLogStr ("[" :: String)
        r = toLogStr ("] " :: String)
        endl = toLogStr ("\n" :: String)

defaultLogSpec :: FileLogSpec
defaultLogSpec =
    FileLogSpec
        "infoproxy.log"
        (10 * 1024 * 1024)
        10
