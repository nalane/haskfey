module Main where

import System.Remote.Monitoring
import System.Environment
import Data.ByteString.Char8 (pack)

import Lib
import Scenes

mainGameMonad :: FeyState ()
mainGameMonad = do
    initGame
    runGame sceneMapping "second"
    endGame
    endLogging

main :: IO ()
main = do
    --forkServer (pack "localhost") 8000
    args <- getArgs
    cfg <- case args of
        [] -> return "feyData/fey.cfg"
        _ -> return $ head args

    newState cfg >>= flip runFeyState mainGameMonad