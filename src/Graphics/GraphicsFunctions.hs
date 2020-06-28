{-| This structure contains the functions for interfacing with the graphics engine.
-}

{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Graphics.GraphicsFunctions (
    GraphicsFunctions, initialize, cleanUp,
    getFunctions
) where

import qualified Graphics.UI.GLFW as GLFW
import qualified Graphics.Rendering.OpenGL as GL
import Graphics.Rendering.OpenGL (($=))
import qualified Vulkan as VK
import Vulkan.Exception

import Foreign.Ptr
import Foreign.C.String

import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BLU
import qualified Data.Vector as V
import Data.Bits
import Data.Maybe

import Control.Lens (makeLenses, (^.))
import Control.Exception
import Control.Monad

import System.IO

import Config
import Graphics.InternalValues

data GraphicsFunctions = GraphicsFunctions {
    _initialize :: IO InternalValues,
    _cleanUp :: InternalValues -> IO ()
}

makeLenses ''GraphicsFunctions

getFunctions :: Config -> GraphicsFunctions
getFunctions cfg =
    let lib = cfg^.graphicsLib
    in case lib of
        OGL -> 
            GraphicsFunctions {
                _initialize = glInitialize,
                _cleanUp = \_ -> return ()
            }
        VK -> 
            GraphicsFunctions {
                _initialize = vkInitialize cfg,
                _cleanUp = vkCleanUp
            }



-- |Sets up OpenGL variables that determine how it renders
glInitialize :: IO InternalValues
glInitialize = do
    GL.clearColor $= GL.Color4 0 0 0 1
    GL.depthFunc $= Just GL.Less
    GL.cullFace $= Just GL.Back
    GL.frontFace $= GL.CCW
    return OpenGL



vkEnableValidationLayers :: Bool
vkEnableValidationLayers = True

vkRequiredLayers :: [BLU.ByteString]
vkRequiredLayers = map BLU.pack ["VK_LAYER_KHRONOS_validation"]

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

    -- Check that required extensions are present
    glfwExtensions <- GLFW.getRequiredInstanceExtensions
    reqExts <-
        if vkEnableValidationLayers then do
            debugExt <- newCString VK.EXT_DEBUG_UTILS_EXTENSION_NAME
            return (glfwExtensions ++ [debugExt])
        else return glfwExtensions
    reqExts <- V.fromList <$> mapM B.packCString reqExts

    (_, exts) <- VK.enumerateInstanceExtensionProperties Nothing
    let extNames = V.map VK.extensionName exts
    let supports = all (`elem` extNames) reqExts
    unless supports $ throw $ VulkanException VK.ERROR_EXTENSION_NOT_PRESENT

    -- Check that required validation layers are present
    layers <- 
        if vkEnableValidationLayers then do
            (_, layers) <- VK.enumerateInstanceLayerProperties
            let layerNames = V.map VK.layerName layers
            let supports = all (`elem` layerNames) vkRequiredLayers
            unless supports $ throw $ VulkanException VK.ERROR_LAYER_NOT_PRESENT
            return $ V.fromList vkRequiredLayers
        else return V.empty

    -- Create instance
    let createInfo = VK.zero {
        VK.applicationInfo = Just appInfo,
        VK.enabledExtensionNames = reqExts,
        VK.enabledLayerNames = layers
    }
    VK.createInstance createInfo Nothing

vkSetupDebugMessenger :: 
    VK.Instance -> 
    FunPtr VK.FN_vkDebugUtilsMessengerCallbackEXT -> 
    IO VK.DebugUtilsMessengerEXT
vkSetupDebugMessenger inst dbgPtr = do
    let dgbCreateInfo = VK.zero {
        VK.messageSeverity =
            VK.DEBUG_UTILS_MESSAGE_SEVERITY_VERBOSE_BIT_EXT 
            .|. VK.DEBUG_UTILS_MESSAGE_SEVERITY_INFO_BIT_EXT
            .|. VK.DEBUG_UTILS_MESSAGE_SEVERITY_WARNING_BIT_EXT 
            .|. VK.DEBUG_UTILS_MESSAGE_SEVERITY_ERROR_BIT_EXT,
        VK.messageType = 
            VK.DEBUG_UTILS_MESSAGE_TYPE_GENERAL_BIT_EXT 
            .|. VK.DEBUG_UTILS_MESSAGE_TYPE_VALIDATION_BIT_EXT 
            .|. VK.DEBUG_UTILS_MESSAGE_TYPE_PERFORMANCE_BIT_EXT,
        VK.pfnUserCallback = dbgPtr
    }
    VK.createDebugUtilsMessengerEXT inst dgbCreateInfo Nothing

vkDbgFunction :: VK.FN_vkDebugUtilsMessengerCallbackEXT
vkDbgFunction sevFlags _ callbackData _ = do
    msg <- VK.message <$> VK.peekCStruct callbackData
    hPutStr stderr "Vulkan Debug Message: "
    hPrint stderr msg
    return VK.TRUE

foreign import ccall "wrapper" createDbgPtr :: 
    VK.FN_vkDebugUtilsMessengerCallbackEXT -> 
    IO (FunPtr VK.FN_vkDebugUtilsMessengerCallbackEXT)

-- |Sets up Vulkan
vkInitialize :: Config -> IO InternalValues
vkInitialize cfg = do
    inst <- vkCreateInstance cfg
    (dbgPtr, dbgMsg) <-
        if vkEnableValidationLayers then do
            p <- createDbgPtr vkDbgFunction
            m <- vkSetupDebugMessenger inst p
            return (p, Just m)
        else return (nullFunPtr, Nothing)
    return $ Vulkan inst dbgPtr dbgMsg

-- |Terminates Vulkan
vkCleanUp :: InternalValues -> IO ()
vkCleanUp (Vulkan inst dbgPtr dbgMsg) = do
    when vkEnableValidationLayers $ do
        VK.destroyDebugUtilsMessengerEXT inst (fromJust dbgMsg) Nothing
        freeHaskellFunPtr dbgPtr
    VK.destroyInstance inst Nothing