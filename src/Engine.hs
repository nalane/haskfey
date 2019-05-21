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

import Graphics.Rendering.OpenGL as GL
import Graphics.UI.GLFW as GLFW

import Data.Foldable
import Data.Maybe
import Data.IORef
import Data.StateVar

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
    sceneRef <- liftIO $ newIORef currScene

    iterateUntil id $ do
        scene <- get sceneRef

        liftIO GLFW.pollEvents
        updateResult <- scene^.update
        case updateResult of
            Nothing -> do
                liftIO $ GL.clear [GL.ColorBuffer, GL.DepthBuffer]
                scene^.draw
    
                liftIO $ GLFW.swapBuffers win
            Just newId -> do
                newScene <- sceneMap newId
                scene^.endScene
                sceneRef $= newScene
                return ()

        liftIO $ GLFW.windowShouldClose win

    scene <- get sceneRef
    scene^.endScene

-- |Cleans up once the game is finished
endGame :: FeyState ()
endGame = do
    win <- getStateVar window
    liftIO $ forM_ win GLFW.destroyWindow
    liftIO GLFW.terminate
    setStateVar window Nothing