{- |Contains functions for initializing graphics system
-}

module Graphics.Initialization (
    initGLFW, initOpenGL
) where

import Graphics.Rendering.OpenGL
import Graphics.UI.GLFW as GLFW

import System.IO
import System.Exit

import Control.Monad
import Control.Lens
import Control.Concurrent.MVar

import Data.Maybe
import Data.Map

import FeyState.Config

-- |Inititalizes GLFW. Returns the GLFW window
initGLFW :: Config -> MVar (Map GLFW.Key Bool) -> IO Window
initGLFW cfg keyMap = do 
    let samples = cfg^.aaSamples
    let title = cfg^.windowTitle

    GLFW.setErrorCallback $ Just $ \_ desc -> hPutStrLn stderr desc
    GLFW.init >>= flip unless (die "FATAL ERROR: Could not init GLFW")

    GLFW.windowHint $ GLFW.WindowHint'ContextVersionMajor 4
    GLFW.windowHint $ GLFW.WindowHint'ContextVersionMinor 1
    GLFW.windowHint $ GLFW.WindowHint'OpenGLProfile GLFW.OpenGLProfile'Core
    GLFW.windowHint $ GLFW.WindowHint'OpenGLForwardCompat True
    GLFW.windowHint $ GLFW.WindowHint'Samples $ Just samples

    (monitor, w, h) <-
        if cfg^.fullScreen
        then do
            m <- fromJust <$> GLFW.getPrimaryMonitor
            mode <- fromJust <$> GLFW.getVideoMode m
            let (VideoMode w h _ _ _ _) = mode
            return (Just m, w, h)
        else return (Nothing, cfg^.width, cfg^.height)

    win <- fromJust <$> GLFW.createWindow w h title monitor Nothing

    GLFW.makeContextCurrent $ Just win
    GLFW.swapInterval 1
    GLFW.setKeyCallback win $ Just (\_ k _ s _ ->
        modifyMVar_ keyMap $ return . insert k (s == GLFW.KeyState'Pressed))
    when (cfg^.hideCursor) $ GLFW.setCursorInputMode win GLFW.CursorInputMode'Disabled

    return win

-- |Sets up OpenGL variables that determine how it renders
initOpenGL :: IO ()
initOpenGL = do
    clearColor $= Color4 0 0 0 1
    depthFunc $= Just Less
    cullFace $= Just Back
    frontFace $= CCW