{-| This structure contains the internal config of the graphics engine.
-}

module Graphics.InternalValues (
    InternalValues(..)
) where

import Vulkan as VK
import Foreign.Ptr

-- |Persistent state of the engine
data InternalValues = 
    OpenGL | 
    Vulkan VK.Instance (FunPtr VK.FN_vkDebugUtilsMessengerCallbackEXT) (Maybe VK.DebugUtilsMessengerEXT) VK.Device VK.Queue