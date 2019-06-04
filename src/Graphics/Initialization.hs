{- |Contains functions for initializing graphics system
-}

module Graphics.Initialization (
    initGLFW, initOpenGL
) where

import Graphics.Rendering.OpenGL
import Graphics.UI.GLFW as GLFW

import FeyState

import System.IO
import System.Exit
import Control.Monad
import Control.Lens
import Data.Maybe

-- |Inititalizes GLFW. Returns the GLFW window
initGLFW :: FeyState Window
initGLFW = do
    w <- getStateVar width
    h <- getStateVar height
    liftIO $ do 
        GLFW.setErrorCallback $ Just $ \_ desc -> hPutStrLn stderr desc
        GLFW.init >>= flip unless (die "FATAL ERROR: Could not init GLFW")

        GLFW.windowHint $ GLFW.WindowHint'ContextVersionMajor 4
        GLFW.windowHint $ GLFW.WindowHint'ContextVersionMinor 1
        GLFW.windowHint $ GLFW.WindowHint'OpenGLProfile GLFW.OpenGLProfile'Core
        GLFW.windowHint $ GLFW.WindowHint'OpenGLForwardCompat True
        GLFW.windowHint $ GLFW.WindowHint'Samples $ Just 16

        maybeWindow <- GLFW.createWindow (fromJust w) (fromJust h) "Demo" Nothing Nothing
        GLFW.makeContextCurrent maybeWindow
        GLFW.swapInterval 1

        when (isNothing maybeWindow) $ die "FATAL ERROR: Could not create window"
        return $ fromJust maybeWindow

-- |Sets up OpenGL variables that determine how it renders
initOpenGL :: FeyState ()
initOpenGL = do
    clearColor $= Color4 0 0 0 1
    depthFunc $= Just Less
    cullFace $= Just Back
    frontFace $= CCW