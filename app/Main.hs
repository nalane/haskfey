module Main where

import System.Remote.Monitoring
import Data.ByteString.Char8

import Lib
import Scenes

mainGameMonad :: FeyState ()
mainGameMonad = do
    initGame
    runGame sceneMapping "main"
    endGame
    endLogging

main :: IO ()
main = do
    forkServer (pack "localhost") 8000
    runFeyState "log.txt" mainGameMonad