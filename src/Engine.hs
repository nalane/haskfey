{- |Contains functions for running the game engine
-}

module Engine (
    isKeyPressed, initGame, runGame, endGame
) where

import FeyState
import Graphics
import Scene
import Config

import System.IO
import System.Exit

import Control.Lens
import Control.Monad.Loops
import Control.Concurrent.MVar
import Control.Monad

import Graphics.Rendering.OpenGL as GL
import Graphics.UI.GLFW as GLFW

import Data.Foldable
import Data.Maybe
import Data.IORef
import Data.StateVar
import qualified Data.Map as M

initGLFW :: Config -> MVar (M.Map GLFW.Key Bool) -> IO GLFW.Window
initGLFW cfg keyMap = do 
    let samples = cfg^.aaSamples
    let title = cfg^.windowTitle
    let graphics = cfg^.graphicsLib

    GLFW.setErrorCallback $ Just $ \_ desc -> hPutStrLn stderr desc
    GLFW.init >>= flip unless (die "FATAL ERROR: Could not init GLFW")

    if graphics == OGL then do
        GLFW.windowHint $ GLFW.WindowHint'ContextVersionMajor 4
        GLFW.windowHint $ GLFW.WindowHint'ContextVersionMinor 1
        GLFW.windowHint $ GLFW.WindowHint'OpenGLProfile GLFW.OpenGLProfile'Core
        GLFW.windowHint $ GLFW.WindowHint'OpenGLForwardCompat True
    else do
        GLFW.windowHint $ GLFW.WindowHint'ClientAPI GLFW.ClientAPI'NoAPI
    GLFW.windowHint $ GLFW.WindowHint'Resizable False
    GLFW.windowHint $ GLFW.WindowHint'Samples $ Just samples

    (monitor, w, h) <-
        if cfg^.fullScreen
        then do
            m <- fromJust <$> GLFW.getPrimaryMonitor
            (GLFW.VideoMode w h _ _ _ _) <- fromJust <$> GLFW.getVideoMode m
            return (Just m, w, h)
        else return (Nothing, cfg^.width, cfg^.height)

    win <- fromJust <$> GLFW.createWindow w h title monitor Nothing

    GLFW.makeContextCurrent $ Just win
    GLFW.swapInterval 1
    GLFW.setKeyCallback win $ Just (\_ k _ s _ ->
        modifyMVar_ keyMap $ return . M.insert k (s == GLFW.KeyState'Pressed))
    when (cfg^.hideCursor) $ GLFW.setCursorInputMode win GLFW.CursorInputMode'Disabled

    return win

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

    let funcs = getFunctions cfg
    setStateVar gfxFunctions $ Just funcs
    iVals <- liftIO (funcs^.initialize)
    setStateVar gfxIValues $ Just iVals

-- |The main game loop
runGame :: (String -> FeyState Scene) -> String -> FeyState ()
runGame sceneMap sceneId = do
    gLib <- getStateVar (config.graphicsLib)
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
    
                when (gLib == OGL) $ liftIO $ GLFW.swapBuffers win
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
    functions <- fromJust <$> getStateVar gfxFunctions
    iVals <- fromJust <$> getStateVar gfxIValues
    liftIO $ (functions^.cleanUp) iVals

    win <- getStateVar window
    liftIO $ forM_ win GLFW.destroyWindow
    liftIO GLFW.terminate
    setStateVar window Nothing