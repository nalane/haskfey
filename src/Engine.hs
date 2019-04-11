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
initGame :: State -> IO State
initGame state = do
    let newState = set width (Just 640) $ set height (Just 480) state
    win <- initGLFW newState
    initOpenGL
    return $ set window (Just win) newState

-- |The main game loop
runGame :: State -> (String -> State -> IO Scene) -> String -> IO State
runGame state sceneMap sceneId = do
    let win = fromJust (state^.window)
    sceneMVar <- sceneMap sceneId state >>= newMVar

    iterateUntil id $ do
        scene <- readMVar sceneMVar

        GLFW.pollEvents
        updateResult <- (scene^.update) state
        case updateResult of
            Nothing -> do
                GL.clear [GL.ColorBuffer, GL.DepthBuffer]
                (scene^.draw) state
    
                GLFW.swapBuffers win
            Just newId -> do
                newScene <- sceneMap newId state
                (scene^.endScene) state
                swapMVar sceneMVar newScene
                return ()

        GLFW.windowShouldClose win

    readMVar sceneMVar >>= \s -> (s^.endScene) state
    return state

-- |Cleans up once the game is finished
endGame :: State -> IO State
endGame state = do
    forM_ (state^.window) GLFW.destroyWindow
    GLFW.terminate
    return $ set window Nothing state