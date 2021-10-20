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
    let cfg = if null args then "feyData/fey.cfg" else head args

    newState cfg >>= runFeyState mainGameMonad