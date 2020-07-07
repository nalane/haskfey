{-| This structure contains the internal config of the graphics engine.
-}

module Graphics.InternalValues (
    InternalValues(..)
) where

import Vulkan as VK
import Graphics.UI.GLFW as GLFW
import Foreign.Ptr

-- |Persistent state of the engine
data InternalValues = 
    OpenGL | 
    Vulkan VK.Instance (Maybe VK.DebugUtilsMessengerEXT) VK.Device [VK.Queue] VK.SurfaceKHR