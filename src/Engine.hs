{- |Contains functions for running the game engine
-}

module Engine (
    initGame, runGame, endGame
) where

import State
import Graphics
import Scene

import Control.Lens
import Control.Monad.Loops
import Control.Concurrent.MVar

import Graphics.Rendering.OpenGL as GL
import Graphics.UI.GLFW as GLFW

import Data.Foldable
import Data.Maybe

-- |Initializes the game engine and sets the window width and height
initGame :: FeyState ()
initGame = do
    setStateVar width $ Just 640
    setStateVar height $ Just 480
    win <- initGLFW
    initOpenGL
    setStateVar window (Just win)

-- |The main game loop
runGame :: (String -> FeyState Scene) -> String -> FeyState ()
runGame sceneMap sceneId = do
    maybeWin <- getStateVar window
    let win = fromJust maybeWin

    currScene <- sceneMap sceneId
    sceneMVar <- execute $ newMVar currScene

    iterateUntil id $ do
        scene <- execute $ readMVar sceneMVar

        execute GLFW.pollEvents
        updateResult <- scene^.update
        case updateResult of
            Nothing -> do
                execute $ GL.clear [GL.ColorBuffer, GL.DepthBuffer]
                scene^.draw
    
                execute $ GLFW.swapBuffers win
            Just newId -> do
                newScene <- sceneMap newId
                scene^.endScene
                execute $ swapMVar sceneMVar newScene
                return ()

        execute $ GLFW.windowShouldClose win

    scene <- execute $ readMVar sceneMVar
    scene^.endScene

-- |Cleans up once the game is finished
endGame :: FeyState ()
endGame = do
    win <- getStateVar window
    execute $ forM_ win GLFW.destroyWindow
    execute GLFW.terminate
    setStateVar window Nothing