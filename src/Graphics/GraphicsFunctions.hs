{-| This structure contains the functions for interfacing with the graphics engine.
-}

{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Graphics.GraphicsFunctions (
    GraphicsFunctions, initialize,
    getFunctions
) where

import qualified Graphics.Rendering.OpenGL as GL
import Graphics.Rendering.OpenGL (($=))
import qualified Vulkan as VK
import qualified Graphics.UI.GLFW as GLFW

import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BLU
import qualified Data.Vector as V

import Control.Lens (makeLenses, (^.))

import Config
import Graphics.InternalValues

data GraphicsFunctions = GraphicsFunctions {
    _initialize :: IO InternalValues
}

makeLenses ''GraphicsFunctions

getFunctions :: Config -> GraphicsFunctions
getFunctions cfg =
    let lib = cfg^.graphicsLib
    in case lib of
        OGL -> 
            GraphicsFunctions {
                _initialize = initOpenGL
            }
        VK -> 
            GraphicsFunctions {
                _initialize = initVulkan cfg
            }



-- |Sets up OpenGL variables that determine how it renders
initOpenGL :: IO InternalValues
initOpenGL = do
    GL.clearColor $= GL.Color4 0 0 0 1
    GL.depthFunc $= Just GL.Less
    GL.cullFace $= Just GL.Back
    GL.frontFace $= GL.CCW
    return OpenGL



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
initVulkan :: Config -> IO InternalValues
initVulkan cfg = do
    inst <- vkCreateInstance cfg
    return $ Vulkan {
        _vkInstance = inst
    }