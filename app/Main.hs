module Main where

import System.Remote.Monitoring
import Data.ByteString.Char8
import Lib
import Scenes

main :: IO ()
main = do
    forkServer (pack "localhost") 8000

    initState <- newState "log.txt" >>= initGame
    endState <- runGame initState sceneMapping "main"
    
    _ <- endGame endState >>= endLogging
    return ()