{-| This structure contains the functions for interfacing with the graphics engine.
-}

{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Graphics.GraphicsFunctions (
    GraphicsFunctions, initialize,
    getFunctions
) where

import qualified Graphics.UI.GLFW as GLFW
import qualified Graphics.Rendering.OpenGL as GL
import Graphics.Rendering.OpenGL (($=))
import qualified Vulkan as VK
import Vulkan.Exception

import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BLU
import qualified Data.Vector as V

import Control.Lens (makeLenses, (^.))
import Control.Exception
import Control.Monad

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



vkEnableValidationLayers :: Bool
vkEnableValidationLayers = True

vkRequiredLayers :: [BLU.ByteString]
vkRequiredLayers = map BLU.pack ["VK_LAYER_KHRONOS_validation"]

vkCheckValidationLayers :: IO Bool
vkCheckValidationLayers = do
    (_, exts) <- VK.enumerateInstanceExtensionProperties Nothing
    let extNames = V.map VK.extensionName exts
    return $ all (`elem` extNames) vkRequiredLayers

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

    layers <- 
        if vkEnableValidationLayers then do
            supports <- vkCheckValidationLayers
            unless supports $ throw $ VulkanException VK.ERROR_VALIDATION_FAILED_EXT
            return $ V.fromList vkRequiredLayers
        else return V.empty
    glfwExtensions <- GLFW.getRequiredInstanceExtensions
    glfwExtensions <- V.fromList <$> mapM B.packCString glfwExtensions

    let createInfo = VK.zero {
        VK.applicationInfo = Just appInfo,
        VK.enabledExtensionNames = glfwExtensions,
        VK.enabledLayerNames = layers
    }
    VK.createInstance createInfo Nothing

-- |Sets up Vulkan
initVulkan :: Config -> IO InternalValues
initVulkan cfg = do
    inst <- vkCreateInstance cfg
    return $ Vulkan {
        _vkInstance = inst
    }