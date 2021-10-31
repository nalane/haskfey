{- |Contains functions for interacting with the log
-}

module FeyState.Logging (
    recordLog, endLogging
) where

import FeyState.FeyState
import FeyState.GameState

import System.IO

-- |Writes to the log
recordLog :: String -> FeyState ()
recordLog msg = do
    file <- getStateVar logFile
    liftIO $ hPutStrLn file msg

-- |Terminates the logging system
endLogging :: FeyState ()
endLogging = do
    file <- getStateVar logFile
    liftIO $ hClose file