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
import Vulkan.CStruct.Extends

import Foreign.Ptr
import Foreign.C.String
import Foreign.Marshal.Utils
import Foreign.Marshal.Alloc
import Foreign.Storable

import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BLU
import qualified Data.Vector as V
import qualified Data.Set as S
import Data.Bits
import Data.Maybe
import Data.Word

import Control.Lens (makeLenses, (^.))
import Control.Exception
import Control.Monad
import Control.Monad.IO.Class

import System.IO

import Config
import Graphics.InternalValues

data GraphicsFunctions m = GraphicsFunctions {
    _initialize :: m InternalValues,
    _cleanUp :: InternalValues -> m ()
}

makeLenses ''GraphicsFunctions

getFunctions :: MonadIO m => Config -> GLFW.Window -> GraphicsFunctions m
getFunctions cfg win =
    let lib = cfg^.graphicsLib
    in case lib of
        OGL -> 
            GraphicsFunctions {
                _initialize = glInitialize,
                _cleanUp = \_ -> return ()
            }
        VK -> 
            GraphicsFunctions {
                _initialize = vkInitialize cfg win,
                _cleanUp = vkCleanUp
            }



-- |Sets up OpenGL variables that determine how it renders
glInitialize :: MonadIO m => m InternalValues
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
vkCreateInstance :: MonadIO m => Config -> m VK.Instance
vkCreateInstance cfg = do
    let appInfo = VK.zero {
        VK.applicationName = Just $ BLU.pack (cfg^.windowTitle),
        VK.applicationVersion = 1,
        VK.engineName = Just $ BLU.pack "HaskFey",
        VK.engineVersion = 1,
        VK.apiVersion = VK.API_VERSION_1_2
    }

    -- Check that required extensions are present
    glfwExtensions <- liftIO GLFW.getRequiredInstanceExtensions
    reqExts <-
        if vkEnableValidationLayers then do
            debugExt <- liftIO $ newCString VK.EXT_DEBUG_UTILS_EXTENSION_NAME
            return (glfwExtensions ++ [debugExt])
        else return glfwExtensions
    reqExts <- liftIO $ V.fromList <$> mapM B.packCString reqExts

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

foreign import ccall unsafe "debugCallback.c &debugCallback"
  debugCallbackPtr :: VK.PFN_vkDebugUtilsMessengerCallbackEXT

vkSetupDebugMessenger :: MonadIO m => VK.Instance -> m VK.DebugUtilsMessengerEXT
vkSetupDebugMessenger inst = do
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
        VK.pfnUserCallback = debugCallbackPtr
    }
    VK.createDebugUtilsMessengerEXT inst dgbCreateInfo Nothing

vkIsDeviceSuitable :: MonadIO m => VK.PhysicalDevice -> m Bool
vkIsDeviceSuitable dev = do
    props <- VK.getPhysicalDeviceProperties dev
    feats <- VK.getPhysicalDeviceFeatures dev
    queue <- VK.getPhysicalDeviceQueueFamilyProperties dev
    return (
        VK.deviceType props == VK.PHYSICAL_DEVICE_TYPE_DISCRETE_GPU
        && VK.tessellationShader feats
        && not (V.null queue)
        && not (V.null $ V.filter (\q -> VK.queueFlags q .&. VK.QUEUE_GRAPHICS_BIT /= zeroBits) queue))

vkPickPhysicalDevice :: MonadIO m => VK.Instance -> m VK.PhysicalDevice
vkPickPhysicalDevice inst = do
    (_, devs) <- VK.enumeratePhysicalDevices inst
    when (V.null devs) $ throw $ VulkanException VK.ERROR_DEVICE_LOST
    V.head <$> V.filterM vkIsDeviceSuitable devs

vkCreateLogicalDevice :: MonadIO m => VK.PhysicalDevice -> VK.SurfaceKHR -> m (VK.Device, [VK.Queue])
vkCreateLogicalDevice dev surface = do
    queues <- VK.getPhysicalDeviceQueueFamilyProperties dev
    let qFlags = V.map VK.queueFlags queues
    let qGraphs = V.map (.&. VK.QUEUE_GRAPHICS_BIT) qFlags

    let graphIndex = V.findIndex (/= zeroBits) qGraphs
    presentIndex <- filterM (\i -> VK.getPhysicalDeviceSurfaceSupportKHR dev i surface) [0..toEnum (V.length queues - 1)]
    when (isNothing graphIndex || null presentIndex) $ throw $ VulkanException VK.ERROR_INCOMPATIBLE_DISPLAY_KHR

    let queueIndices = S.toList $ S.fromList [toEnum $ fromJust graphIndex, head presentIndex]
    let queueCreateInfos = map (\i -> VK.zero {
        VK.queueFamilyIndex = i,
        VK.queuePriorities = V.fromList [1.0]
    }) queueIndices

    enabledLayers <-
        if vkEnableValidationLayers then return $ V.fromList vkRequiredLayers
        else return V.empty
    let createInfo = VK.zero {
        VK.queueCreateInfos = V.fromList $ map SomeStruct queueCreateInfos,
        VK.enabledFeatures = Just VK.zero,
        VK.enabledExtensionNames = V.empty,
        VK.enabledLayerNames = enabledLayers
    }
    
    vDev <- VK.createDevice dev createInfo Nothing
    queues <- mapM (\i -> VK.getDeviceQueue vDev i 0) queueIndices
    return (vDev, queues)

vkCreateSurface :: MonadIO m => VK.Instance -> GLFW.Window -> m VK.SurfaceKHR
vkCreateSurface inst win = liftIO $ do
    let instPtr = VK.instanceHandle inst

    surfacePtr <- malloc :: IO (Ptr VK.SurfaceKHR)
    res <- VK.Result <$> GLFW.createWindowSurface instPtr win nullPtr surfacePtr
    when (res /= VK.SUCCESS) $ throw $ VulkanException res

    surface <- peek surfacePtr
    free surfacePtr
    return surface

-- |Sets up Vulkan
vkInitialize :: MonadIO m => Config -> GLFW.Window -> m InternalValues
vkInitialize cfg win = do
    inst <- vkCreateInstance cfg
    dbgMsg <-
        if vkEnableValidationLayers then Just <$> vkSetupDebugMessenger inst
        else return Nothing
    surface <- vkCreateSurface inst win
    dev <- vkPickPhysicalDevice inst
    (dev, queues) <- vkCreateLogicalDevice dev surface
    return $ Vulkan inst dbgMsg dev queues surface

-- |Terminates Vulkan
vkCleanUp :: MonadIO m => InternalValues -> m ()
vkCleanUp (Vulkan inst dbgMsg dev _ surface) = do
    VK.destroyDevice dev Nothing
    when vkEnableValidationLayers $ VK.destroyDebugUtilsMessengerEXT inst (fromJust dbgMsg) Nothing
    VK.destroySurfaceKHR inst surface Nothing
    VK.destroyInstance inst Nothing