{- |Contains functions for running the game engine
-}

module Engine (
    isKeyPressed, initGame, runGame, endGame
) where

import FeyState
import Graphics
import Scene

import Control.Lens
import Control.Monad.Loops
import Control.Concurrent.MVar

import Graphics.Rendering.OpenGL as GL
import Graphics.UI.GLFW as GLFW

import Data.Foldable
import Data.Maybe
import Data.IORef
import Data.StateVar
import qualified Data.Map as M

-- |Determines if the given key is in a pressed state or not
isKeyPressed :: GLFW.Key -> FeyState Bool
isKeyPressed k = do
    keyMap <- getStateVar keyState >>= (liftIO . readMVar)
    
    let res = M.lookup k keyMap
    case res of
        Just True -> return True
        _ -> return False

-- |Initializes the game engine and sets the window width and height
initGame :: FeyState ()
initGame = do
    cfg <- getStateVar config
    k <- getStateVar keyState

    win <- liftIO $ initGLFW cfg k
    setStateVar window $ Just win

    if cfg^.graphicsLib == OpenGL 
        then liftIO initOpenGL 
        else liftIO $ initVulkan cfg

-- |The main game loop
runGame :: (String -> FeyState Scene) -> String -> FeyState ()
runGame sceneMap sceneId = do
    win <- fromJust <$> getStateVar window

    currScene <- sceneMap sceneId
    sceneRef <- liftIO $ newIORef currScene

    iterateUntil id $ do
        scene <- get sceneRef

        liftIO GLFW.pollEvents
        updateResult <- scene^.updateScene
        case updateResult of
            Nothing -> do
                liftIO $ GL.clear [GL.ColorBuffer, GL.DepthBuffer]
                scene^.drawScene
    
                liftIO $ GLFW.swapBuffers win
            Just newId -> do
                ks <- getStateVar keyState
                liftIO $ swapMVar ks M.empty

                newScene <- sceneMap newId
                scene^.endScene
                sceneRef $= newScene

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