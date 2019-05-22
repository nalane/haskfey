module FeyState.Logging (
    recordLog, endLogging
) where

import FeyState.FeyState
import FeyState.State

import System.IO

import Control.Monad

-- |Writes to the log
recordLog :: String -> FeyState ()
recordLog msg = do
    maybeFile <- getStateVar logFile
    liftIO $ forM_ maybeFile $ flip hPutStrLn msg

-- |Terminates the logging system
endLogging :: FeyState ()
endLogging = do
    maybeFile <- getStateVar logFile
    liftIO $ forM_ maybeFile hClose
    setStateVar logPath Nothing 
    setStateVar logFile Nothing