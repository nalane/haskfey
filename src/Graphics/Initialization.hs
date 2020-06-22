{-# LANGUAGE DuplicateRecordFields #-}

{- |Contains functions for initializing graphics system
-}

module Graphics.Initialization (
    initGLFW, initOpenGL, initVulkan
) where

import qualified Graphics.Rendering.OpenGL as GL
import Graphics.Rendering.OpenGL (($=))
import qualified Vulkan as VK
import qualified Graphics.UI.GLFW as GLFW

import System.IO
import System.Exit

import Control.Monad
import Control.Lens
import Control.Concurrent.MVar

import Data.Maybe
import Data.Map
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BLU
import qualified Data.Vector as V

import FeyState.Config

-- |Inititalizes GLFW. Returns the GLFW window
initGLFW :: Config -> MVar (Map GLFW.Key Bool) -> IO GLFW.Window
initGLFW cfg keyMap = do 
    let samples = cfg^.aaSamples
    let title = cfg^.windowTitle
    let graphics = cfg^.graphicsLib

    GLFW.setErrorCallback $ Just $ \_ desc -> hPutStrLn stderr desc
    GLFW.init >>= flip unless (die "FATAL ERROR: Could not init GLFW")

    if graphics == OpenGL then do
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
        modifyMVar_ keyMap $ return . insert k (s == GLFW.KeyState'Pressed))
    when (cfg^.hideCursor) $ GLFW.setCursorInputMode win GLFW.CursorInputMode'Disabled

    return win

-- |Sets up OpenGL variables that determine how it renders
initOpenGL :: IO ()
initOpenGL = do
    GL.clearColor $= GL.Color4 0 0 0 1
    GL.depthFunc $= Just GL.Less
    GL.cullFace $= Just GL.Back
    GL.frontFace $= GL.CCW



-- Create a Vulkan instance
vkCreateInstance :: Config -> IO VK.Instance
vkCreateInstance cfg = do
    let appInfo = VK.zero {
        VK.applicationName = Just $ BLU.pack (cfg^.windowTitle),
        VK.applicationVersion = 1,
        VK.engineName = Just $ BLU.pack "HaskFey",
        VK.engineVersion = 1,
        VK.apiVersion = VK.API_VERSION_1_2
    }

    glfwExtensions <- GLFW.getRequiredInstanceExtensions
    glfwExtensions <- V.fromList <$> mapM B.packCString glfwExtensions
    let createInfo = VK.zero {
        VK.applicationInfo = Just appInfo,
        VK.enabledExtensionNames = glfwExtensions,
        VK.enabledLayerNames = V.empty
    }
    inst <- VK.createInstance createInfo Nothing

    (res, exts) <- VK.enumerateInstanceExtensionProperties Nothing
    V.mapM_ print exts

    return inst

-- |Sets up Vulkan
initVulkan :: Config -> IO ()
initVulkan cfg = do
    _ <- vkCreateInstance cfg
    return ()