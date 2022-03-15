{-| This structure contains the functions for interfacing with the graphics engine.
-}

{-# LANGUAGE DuplicateRecordFields #-}

module Graphics.GraphicsFunctions (
    initGraphics, endGraphics
) where

import qualified Graphics.UI.GLFW as GLFW
import qualified Graphics.Rendering.OpenGL as GL
import Graphics.Rendering.OpenGL (($=))

import qualified Vulkan as VK
import Vulkan.Exception
import Vulkan.CStruct.Extends
import Vulkan.Zero (zero)

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

foreign import ccall unsafe "debugCallback.c &debugCallback"
    debugCallbackPtr :: VK.PFN_vkDebugUtilsMessengerCallbackEXT

vkEnableValidationLayers :: Bool
vkEnableValidationLayers = True

vkRequiredLayers :: [BLU.ByteString]
vkRequiredLayers = map BLU.pack ["VK_LAYER_KHRONOS_validation"]

vkDeviceExtensions :: [BLU.ByteString]
vkDeviceExtensions = map BLU.pack [VK.KHR_SWAPCHAIN_EXTENSION_NAME]

-- Create a Vulkan instance
vkCreateInstance :: Config -> IO VK.Instance
vkCreateInstance cfg = do
    let appInfo = zero {
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
    let createInfo = zero {
        VK.applicationInfo = Just appInfo,
        VK.enabledExtensionNames = reqExts,
        VK.enabledLayerNames = layers
    }
    VK.createInstance createInfo Nothing

vkSetupDebugMessenger :: VK.Instance -> IO VK.DebugUtilsMessengerEXT
vkSetupDebugMessenger inst = do
    let dgbCreateInfo = zero {
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

vkQuerySwapChainSupport :: 
    VK.PhysicalDevice -> 
    VK.SurfaceKHR -> 
    IO (VK.SurfaceCapabilitiesKHR, V.Vector VK.SurfaceFormatKHR, V.Vector VK.PresentModeKHR)
vkQuerySwapChainSupport dev surface = do
    caps <- VK.getPhysicalDeviceSurfaceCapabilitiesKHR dev surface
    (_, fmts) <- VK.getPhysicalDeviceSurfaceFormatsKHR dev surface
    (_, modes) <- VK.getPhysicalDeviceSurfacePresentModesKHR dev surface
    return (caps, fmts, modes)

vkIsDeviceSuitable :: VK.SurfaceKHR -> VK.PhysicalDevice -> IO Bool
vkIsDeviceSuitable surface dev = do
    props <- VK.getPhysicalDeviceProperties dev
    feats <- VK.getPhysicalDeviceFeatures dev
    queue <- VK.getPhysicalDeviceQueueFamilyProperties dev

    (_, extProps) <- VK.enumerateDeviceExtensionProperties dev Nothing
    let extensionsSupported = all (`V.elem` V.map VK.extensionName extProps) vkDeviceExtensions

    if extensionsSupported then do
        (_, swapFormats, swapModes) <- vkQuerySwapChainSupport dev surface
        return (
            VK.deviceType props == VK.PHYSICAL_DEVICE_TYPE_DISCRETE_GPU
            && VK.tessellationShader feats
            && not (V.null queue)
            && not (V.null $ V.filter (\q -> VK.queueFlags q .&. VK.QUEUE_GRAPHICS_BIT /= zeroBits) queue)
            && not (V.null swapFormats)
            && not (V.null swapModes))
    else return False

vkPickPhysicalDevice :: VK.Instance -> VK.SurfaceKHR -> IO VK.PhysicalDevice
vkPickPhysicalDevice inst surface = do
    (_, devs) <- VK.enumeratePhysicalDevices inst
    when (V.null devs) $ throw $ VulkanException VK.ERROR_DEVICE_LOST
    V.head <$> V.filterM (vkIsDeviceSuitable surface) devs

vkCreateLogicalDevice :: VK.PhysicalDevice -> VK.SurfaceKHR -> IO (VK.Device, [VK.Queue], [Word32])
vkCreateLogicalDevice dev surface = do
    queues <- VK.getPhysicalDeviceQueueFamilyProperties dev
    let qFlags = V.map VK.queueFlags queues
    let qGraphs = V.map (.&. VK.QUEUE_GRAPHICS_BIT) qFlags

    let graphIndex = V.findIndex (/= zeroBits) qGraphs
    presentIndex <- filterM (\i -> VK.getPhysicalDeviceSurfaceSupportKHR dev i surface) [0..toEnum (V.length queues - 1)]
    when (isNothing graphIndex || null presentIndex) $ throw $ VulkanException VK.ERROR_INCOMPATIBLE_DISPLAY_KHR

    let queueIndices = S.toList $ S.fromList [toEnum $ fromJust graphIndex, head presentIndex]
    let queueCreateInfos = map (\i -> zero {
        VK.queueFamilyIndex = i,
        VK.queuePriorities = V.fromList [1.0]
    }) queueIndices

    enabledLayers <-
        if vkEnableValidationLayers then return $ V.fromList vkRequiredLayers
        else return V.empty
    let createInfo = zero {
        VK.queueCreateInfos = V.fromList $ map SomeStruct queueCreateInfos,
        VK.enabledFeatures = Just zero,
        VK.enabledExtensionNames = V.fromList vkDeviceExtensions,
        VK.enabledLayerNames = enabledLayers
    }
    
    vDev <- VK.createDevice dev createInfo Nothing
    queues <- mapM (\i -> VK.getDeviceQueue vDev i 0) queueIndices
    return (vDev, queues, queueIndices)

vkCreateSurface :: VK.Instance -> GLFW.Window -> IO VK.SurfaceKHR
vkCreateSurface inst win = liftIO $ do
    let instPtr = VK.instanceHandle inst

    surfacePtr <- malloc :: IO (Ptr VK.SurfaceKHR)
    res <- VK.Result <$> GLFW.createWindowSurface instPtr win nullPtr surfacePtr
    when (res /= VK.SUCCESS) $ throw $ VulkanException res

    surface <- peek surfacePtr
    free surfacePtr
    return surface

vkChooseSwapSurfaceFormat :: V.Vector VK.SurfaceFormatKHR -> VK.SurfaceFormatKHR
vkChooseSwapSurfaceFormat formats 
    | V.null acceptedFormats = V.head formats
    | otherwise = V.head acceptedFormats where
        acceptedFormats = V.filter (\(VK.SurfaceFormatKHR format colorSpace) -> 
            (format == VK.FORMAT_B8G8R8A8_SRGB) 
            && (colorSpace == VK.COLOR_SPACE_SRGB_NONLINEAR_KHR)) formats

vkChooseSwapPresentMode :: V.Vector VK.PresentModeKHR -> VK.PresentModeKHR
vkChooseSwapPresentMode modes
    | VK.PRESENT_MODE_MAILBOX_KHR `V.elem` modes = VK.PRESENT_MODE_MAILBOX_KHR
    | otherwise = VK.PRESENT_MODE_FIFO_KHR

vkChooseSwapExtent :: VK.SurfaceCapabilitiesKHR -> Config -> VK.Extent2D
vkChooseSwapExtent (VK.SurfaceCapabilitiesKHR _ _ currentExtent minImageExtent maxImageExtent _ _ _ _ _) cfg =
    if w /= maxBound then currentExtent
    else VK.Extent2D actualWidth actualHeight where
        (VK.Extent2D w h) = currentExtent
        (VK.Extent2D maxExtentW maxExtentH) = maxImageExtent
        (VK.Extent2D minExtentW minExtentH) = minImageExtent
        actualWidth = max minExtentW $ min maxExtentW $ toEnum (cfg^.width)
        actualHeight = max minExtentH $ min maxExtentH $ toEnum (cfg^.height)

vkCreateSwapChain ::
    VK.PhysicalDevice -> 
    VK.Device -> 
    VK.SurfaceKHR ->
    [Word32] -> 
    Config -> 
    IO (VK.SwapchainKHR, V.Vector VK.Image, VK.Format, VK.Extent2D)
vkCreateSwapChain pDev vDev surface queues cfg = do
    (caps, fmts, modes) <- vkQuerySwapChainSupport pDev surface
    let (VK.SurfaceFormatKHR format colorSpace) = vkChooseSwapSurfaceFormat fmts
    let presentMode = vkChooseSwapPresentMode modes
    let extent = vkChooseSwapExtent caps cfg

    let (VK.SurfaceCapabilitiesKHR minImageCount _ _ _ _ _ _ currentTransform _ _) = caps
    let imageCount = minImageCount + 1

    let (imageSharingMode, queueFamilyIndices) = if (length queues > 1) && (head queues /= (queues !! 1))
        then (VK.SHARING_MODE_CONCURRENT, V.fromList queues)
        else (VK.SHARING_MODE_EXCLUSIVE, V.empty)

    let createInfo = zero {
        VK.surface = surface,
        VK.minImageCount = imageCount,
        VK.imageFormat = format,
        VK.imageColorSpace = colorSpace,
        VK.imageExtent = extent,
        VK.imageArrayLayers = 1,
        VK.imageUsage = VK.IMAGE_USAGE_COLOR_ATTACHMENT_BIT,
        VK.imageSharingMode = imageSharingMode,
        VK.queueFamilyIndices = queueFamilyIndices,
        VK.preTransform = currentTransform,
        VK.compositeAlpha = VK.COMPOSITE_ALPHA_OPAQUE_BIT_KHR,
        VK.presentMode = presentMode,
        VK.clipped = True
    }

    swapchain <- VK.createSwapchainKHR vDev createInfo Nothing
    (_, images) <- VK.getSwapchainImagesKHR vDev swapchain
    return (swapchain, images, format, extent)

vkCreateImageViews :: V.Vector VK.Image -> VK.Format -> VK.Device -> IO (V.Vector VK.ImageView)
vkCreateImageViews images format dev = V.mapM (\image -> do
    let components = zero {
        VK.r = VK.COMPONENT_SWIZZLE_IDENTITY,
        VK.g = VK.COMPONENT_SWIZZLE_IDENTITY,
        VK.b = VK.COMPONENT_SWIZZLE_IDENTITY,
        VK.a = VK.COMPONENT_SWIZZLE_IDENTITY
    }
    let subresourceRange = zero {
        VK.aspectMask = VK.IMAGE_ASPECT_COLOR_BIT,
        VK.baseMipLevel = 0,
        VK.levelCount = 1,
        VK.baseArrayLayer = 0,
        VK.layerCount = 1
    }
    let createInfo = zero {
        VK.image = image,
        VK.viewType = VK.IMAGE_VIEW_TYPE_2D,
        VK.format = format,
        VK.components = components,
        VK.subresourceRange = subresourceRange
    }
    VK.createImageView dev createInfo Nothing) images

----------------- PUBLIC FUNCTIONS -----------------

initGraphics :: GraphicsLib -> Config -> GLFW.Window -> IO InternalValues 
initGraphics OGL _ _ = do
    GL.clearColor $= GL.Color4 0 0 0 1
    GL.depthFunc $= Just GL.Less
    GL.cullFace $= Just GL.Back
    GL.frontFace $= GL.CCW
    return OpenGL
initGraphics VK cfg win = do
    inst <- vkCreateInstance cfg
    dbgMsg <-
        if vkEnableValidationLayers then Just <$> vkSetupDebugMessenger inst
        else return Nothing
    surface <- vkCreateSurface inst win
    pDev <- vkPickPhysicalDevice inst surface
    (vDev, queues, queueIndices) <- vkCreateLogicalDevice pDev surface
    (swapchain, images, format, extent) <- vkCreateSwapChain pDev vDev surface queueIndices cfg
    imageViews <- vkCreateImageViews images format vDev
    return $ Vulkan inst dbgMsg vDev queues surface swapchain images format extent imageViews

endGraphics :: InternalValues -> IO ()
endGraphics OpenGL = return ()
endGraphics (Vulkan inst dbgMsg dev _ surface swapchain _ _ _ imageViews) = do
    V.forM_ imageViews (\view -> VK.destroyImageView dev view Nothing)
    VK.destroySwapchainKHR dev swapchain Nothing
    VK.destroyDevice dev Nothing
    when vkEnableValidationLayers $ VK.destroyDebugUtilsMessengerEXT inst (fromJust dbgMsg) Nothing
    VK.destroySurfaceKHR inst surface Nothing
    VK.destroyInstance inst Nothing